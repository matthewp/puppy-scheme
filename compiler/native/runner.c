#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <wasmtime.h>
#include <wasi.h>

#if defined(__APPLE__)
#include <mach-o/dyld.h>
#elif defined(__FreeBSD__)
#include <sys/types.h>
#include <sys/sysctl.h>
#endif

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

static void read_payload(uint8_t **out, size_t *out_len) {
    // Read the embedded .wasm from the end of our own executable.
    // Layout: [exe][wasm payload][8-byte little-endian payload size]
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

    // Read the 8-byte trailer
    fseek(self, -8, SEEK_END);
    uint64_t payload_size;
    if (fread(&payload_size, 8, 1, self) != 1) {
        fprintf(stderr, "error: cannot read payload trailer\n");
        exit(1);
    }

    // Sanity check
    long file_size = ftell(self);
    if ((int64_t)payload_size <= 0 || (int64_t)payload_size > file_size - 8) {
        fprintf(stderr, "error: no embedded module found\n");
        exit(1);
    }

    // Read the payload
    uint8_t *buf = malloc(payload_size);
    if (!buf) {
        fprintf(stderr, "error: out of memory\n");
        exit(1);
    }
    fseek(self, -(8 + (long)payload_size), SEEK_END);
    if (fread(buf, 1, payload_size, self) != payload_size) {
        fprintf(stderr, "error: cannot read payload\n");
        exit(1);
    }
    fclose(self);

    *out = buf;
    *out_len = payload_size;
}

int main(int argc, char *argv[]) {
    (void)argc;
    (void)argv;

    // Read embedded .wasm payload
    uint8_t *payload;
    size_t payload_len;
    read_payload(&payload, &payload_len);

    // Create engine with GC enabled
    wasm_config_t *config = wasm_config_new();
    wasmtime_config_wasm_gc_set(config, true);
    wasm_engine_t *engine = wasm_engine_new_with_config(config);

    // Compile the module
    wasmtime_module_t *module;
    wasmtime_error_t *error =
        wasmtime_module_new(engine, payload, payload_len, &module);
    if (error)
        exit_with_error("failed to compile module", error, NULL);
    free(payload);

    // Create store
    wasmtime_store_t *store = wasmtime_store_new(engine, NULL, NULL);
    wasmtime_context_t *context = wasmtime_store_context(store);

    // Configure WASI
    wasi_config_t *wasi_config = wasi_config_new();
    wasi_config_inherit_stdin(wasi_config);
    wasi_config_inherit_stdout(wasi_config);
    wasi_config_inherit_stderr(wasi_config);
    wasi_config_inherit_argv(wasi_config);
    wasi_config_inherit_env(wasi_config);
    wasmtime_context_set_wasi(context, wasi_config);

    // Create linker and define WASI
    wasmtime_linker_t *linker = wasmtime_linker_new(engine);
    error = wasmtime_linker_define_wasi(linker);
    if (error)
        exit_with_error("failed to define WASI", error, NULL);

    // Instantiate
    wasmtime_instance_t instance;
    wasm_trap_t *trap = NULL;
    error =
        wasmtime_linker_instantiate(linker, context, module, &instance, &trap);
    if (error || trap)
        exit_with_error("failed to instantiate", error, trap);

    // Call _start
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

    // Cleanup
    wasmtime_linker_delete(linker);
    wasmtime_store_delete(store);
    wasmtime_module_delete(module);
    wasm_engine_delete(engine);
    return exit_status;
}
