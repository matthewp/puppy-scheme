#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <wasmtime.h>
#include <wasmtime/component.h>
#include <wasi.h>

#if defined(__APPLE__)
#include <mach-o/dyld.h>
#elif defined(__FreeBSD__)
#include <sys/types.h>
#include <sys/sysctl.h>
#endif

#define MAGIC "PUPYPACK"

static void exit_with_error(const char *msg, wasmtime_error_t *error,
                            wasm_trap_t *trap) {
    fprintf(stderr, "error: %s\n", msg);
    if (error) {
        wasm_byte_vec_t emsg;
        wasmtime_error_message(error, &emsg);
        fprintf(stderr, "%.*s\n", (int)emsg.size, emsg.data);
        wasm_byte_vec_delete(&emsg);
        wasmtime_error_delete(error);
    }
    if (trap) {
        wasm_byte_vec_t emsg;
        wasm_trap_message(trap, &emsg);
        fprintf(stderr, "%.*s\n", (int)emsg.size, emsg.data);
        wasm_byte_vec_delete(&emsg);
        wasm_trap_delete(trap);
    }
    exit(1);
}

typedef struct {
    char *host_path;
    char *guest_path;
} dir_mapping_t;

static void read_sections(uint8_t **cwasm_out, size_t *cwasm_len,
                          uint8_t **config_out, size_t *config_len,
                          uint8_t *type_out) {
#if defined(__linux__)
    FILE *self = fopen("/proc/self/exe", "rb");
#elif defined(__APPLE__)
    char self_path[4096];
    uint32_t self_path_size = sizeof(self_path);
    _NSGetExecutablePath(self_path, &self_path_size);
    FILE *self = fopen(self_path, "rb");
#elif defined(__FreeBSD__)
    char self_path[4096];
    size_t self_path_size = sizeof(self_path);
    int mib[4] = {CTL_KERN, KERN_PROC, KERN_PROC_PATHNAME, -1};
    sysctl(mib, 4, self_path, &self_path_size, NULL, 0);
    FILE *self = fopen(self_path, "rb");
#endif
    if (!self) {
        fprintf(stderr, "error: cannot open own executable\n");
        exit(1);
    }

    // Read 25-byte trailer: cwasm_size(8) + config_size(8) + type(1) + magic(8)
    fseek(self, -25, SEEK_END);
    uint64_t cwasm_size, config_size;
    uint8_t type;
    char magic[8];
    if (fread(&cwasm_size, 8, 1, self) != 1 ||
        fread(&config_size, 8, 1, self) != 1 ||
        fread(&type, 1, 1, self) != 1 ||
        fread(magic, 8, 1, self) != 1) {
        fprintf(stderr, "error: cannot read trailer\n");
        exit(1);
    }

    if (memcmp(magic, MAGIC, 8) != 0) {
        fprintf(stderr, "error: not a puppypack binary\n");
        exit(1);
    }

    // Read cwasm payload
    uint8_t *cwasm = malloc(cwasm_size);
    if (!cwasm) {
        fprintf(stderr, "error: out of memory\n");
        exit(1);
    }
    fseek(self, -(25 + (long)config_size + (long)cwasm_size), SEEK_END);
    if (fread(cwasm, 1, cwasm_size, self) != cwasm_size) {
        fprintf(stderr, "error: cannot read cwasm payload\n");
        exit(1);
    }

    // Read config section
    uint8_t *config = malloc(config_size);
    if (!config) {
        fprintf(stderr, "error: out of memory\n");
        exit(1);
    }
    if (fread(config, 1, config_size, self) != config_size) {
        fprintf(stderr, "error: cannot read config\n");
        exit(1);
    }

    fclose(self);

    *cwasm_out = cwasm;
    *cwasm_len = cwasm_size;
    *config_out = config;
    *config_len = config_size;
    *type_out = type;
}

static int parse_config(const uint8_t *config, size_t config_len,
                        dir_mapping_t **dirs_out) {
    if (config_len < 4) {
        *dirs_out = NULL;
        return 0;
    }

    uint32_t count;
    memcpy(&count, config, 4);
    if (count == 0) {
        *dirs_out = NULL;
        return 0;
    }

    dir_mapping_t *dirs = calloc(count, sizeof(dir_mapping_t));
    const uint8_t *p = config + 4;
    const uint8_t *end = config + config_len;

    for (uint32_t i = 0; i < count; i++) {
        // host_path: NUL-terminated
        dirs[i].host_path = (char *)p;
        while (p < end && *p) p++;
        if (p >= end) {
            fprintf(stderr, "error: malformed config\n");
            exit(1);
        }
        p++; // skip NUL

        // guest_path: NUL-terminated
        dirs[i].guest_path = (char *)p;
        while (p < end && *p) p++;
        if (p >= end) {
            fprintf(stderr, "error: malformed config\n");
            exit(1);
        }
        p++; // skip NUL
    }

    *dirs_out = dirs;
    return (int)count;
}

static void configure_wasi(wasmtime_context_t *context,
                           dir_mapping_t *dirs, int num_dirs) {
    wasi_config_t *wasi_config = wasi_config_new();
    wasi_config_inherit_stdin(wasi_config);
    wasi_config_inherit_stdout(wasi_config);
    wasi_config_inherit_stderr(wasi_config);
    wasi_config_inherit_argv(wasi_config);
    wasi_config_inherit_env(wasi_config);
    for (int i = 0; i < num_dirs; i++) {
        bool ok = wasi_config_preopen_dir(wasi_config, dirs[i].host_path,
            dirs[i].guest_path,
            WASMTIME_WASI_DIR_PERMS_READ | WASMTIME_WASI_DIR_PERMS_WRITE,
            WASMTIME_WASI_FILE_PERMS_READ | WASMTIME_WASI_FILE_PERMS_WRITE);
        if (!ok) {
            fprintf(stderr, "error: failed to preopen '%s' as '%s'\n",
                    dirs[i].host_path, dirs[i].guest_path);
            exit(1);
        }
    }
    if (num_dirs > 0) {
        char cwd[4096];
        if (getcwd(cwd, sizeof(cwd))) {
            wasi_config_preopen_dir(wasi_config, cwd, ".",
                WASMTIME_WASI_DIR_PERMS_READ | WASMTIME_WASI_DIR_PERMS_WRITE,
                WASMTIME_WASI_FILE_PERMS_READ | WASMTIME_WASI_FILE_PERMS_WRITE);
        }
    }
    wasmtime_context_set_wasi(context, wasi_config);
}

static int run_module(wasm_engine_t *engine, uint8_t *cwasm, size_t cwasm_len,
                      dir_mapping_t *dirs, int num_dirs) {
    wasmtime_module_t *module;
    wasmtime_error_t *error =
        wasmtime_module_deserialize(engine, cwasm, cwasm_len, &module);
    if (error)
        exit_with_error("failed to deserialize module", error, NULL);
    free(cwasm);

    wasmtime_store_t *store = wasmtime_store_new(engine, NULL, NULL);
    wasmtime_context_t *context = wasmtime_store_context(store);
    configure_wasi(context, dirs, num_dirs);

    wasmtime_linker_t *linker = wasmtime_linker_new(engine);
    error = wasmtime_linker_define_wasi(linker);
    if (error)
        exit_with_error("failed to define WASI", error, NULL);

    wasmtime_instance_t instance;
    wasm_trap_t *trap = NULL;
    error =
        wasmtime_linker_instantiate(linker, context, module, &instance, &trap);
    if (error || trap)
        exit_with_error("failed to instantiate", error, trap);

    wasmtime_extern_t start_extern;
    bool found = wasmtime_instance_export_get(context, &instance, "_start", 6,
                                              &start_extern);
    if (!found || start_extern.kind != WASMTIME_EXTERN_FUNC) {
        fprintf(stderr, "error: _start not found\n");
        exit(1);
    }

    error = wasmtime_func_call(context, &start_extern.of.func, NULL, 0, NULL, 0,
                               &trap);
    int exit_status = 0;
    if (error) {
        if (wasmtime_error_exit_status(error, &exit_status)) {
            wasmtime_error_delete(error);
        } else {
            exit_with_error("_start failed", error, NULL);
        }
    }
    if (trap)
        exit_with_error("_start trapped", NULL, trap);

    wasmtime_linker_delete(linker);
    wasmtime_store_delete(store);
    wasmtime_module_delete(module);
    return exit_status;
}

static int run_component(wasm_engine_t *engine, uint8_t *cwasm, size_t cwasm_len,
                         dir_mapping_t *dirs, int num_dirs) {
    wasmtime_component_t *component;
    wasmtime_error_t *error =
        wasmtime_component_deserialize(engine, cwasm, cwasm_len, &component);
    if (error)
        exit_with_error("failed to deserialize component", error, NULL);
    free(cwasm);

    wasmtime_store_t *store = wasmtime_store_new(engine, NULL, NULL);
    wasmtime_context_t *context = wasmtime_store_context(store);
    configure_wasi(context, dirs, num_dirs);

    wasmtime_component_linker_t *linker =
        wasmtime_component_linker_new(engine);
    error = wasmtime_component_linker_add_wasip2(linker);
    if (error)
        exit_with_error("failed to add WASI P2", error, NULL);

    wasmtime_component_instance_t instance;
    error = wasmtime_component_linker_instantiate(linker, context, component,
                                                  &instance);
    if (error)
        exit_with_error("failed to instantiate component", error, NULL);

    // Look up wasi:cli/run@0.2.0 "run" export
    wasmtime_component_export_index_t *run_ns_idx =
        wasmtime_component_get_export_index(component, NULL,
            "wasi:cli/run@0.2.0", 18);
    if (!run_ns_idx) {
        fprintf(stderr, "error: wasi:cli/run@0.2.0 export not found\n");
        exit(1);
    }
    wasmtime_component_export_index_t *run_fn_idx =
        wasmtime_component_get_export_index(component, run_ns_idx,
            "run", 3);
    wasmtime_component_export_index_delete(run_ns_idx);
    if (!run_fn_idx) {
        fprintf(stderr, "error: run function not found\n");
        exit(1);
    }

    wasmtime_component_func_t func;
    bool found = wasmtime_component_instance_get_func(&instance, context,
        run_fn_idx, &func);
    wasmtime_component_export_index_delete(run_fn_idx);
    if (!found) {
        fprintf(stderr, "error: run function not exported\n");
        exit(1);
    }

    wasmtime_component_val_t result;
    error = wasmtime_component_func_call(&func, context, NULL, 0, &result, 1);
    int exit_status = 0;
    if (error) {
        if (wasmtime_error_exit_status(error, &exit_status)) {
            wasmtime_error_delete(error);
        } else {
            exit_with_error("run failed", error, NULL);
        }
    } else {
        error = wasmtime_component_func_post_return(&func, context);
        if (error)
            exit_with_error("post_return failed", error, NULL);
    }

    wasmtime_component_linker_delete(linker);
    wasmtime_store_delete(store);
    wasmtime_component_delete(component);
    return exit_status;
}

int main(int argc, char *argv[]) {
    (void)argc;
    (void)argv;

    uint8_t *cwasm, *config;
    size_t cwasm_len, config_len;
    uint8_t type;
    read_sections(&cwasm, &cwasm_len, &config, &config_len, &type);

    dir_mapping_t *dirs;
    int num_dirs = parse_config(config, config_len, &dirs);

    wasm_config_t *wasm_cfg = wasm_config_new();
    wasmtime_config_wasm_gc_set(wasm_cfg, true);
    if (type == 1)
        wasmtime_config_wasm_component_model_set(wasm_cfg, true);
    wasm_engine_t *engine = wasm_engine_new_with_config(wasm_cfg);

    int exit_status;
    if (type == 1)
        exit_status = run_component(engine, cwasm, cwasm_len, dirs, num_dirs);
    else
        exit_status = run_module(engine, cwasm, cwasm_len, dirs, num_dirs);

    free(dirs);
    free(config);
    wasm_engine_delete(engine);
    return exit_status;
}
