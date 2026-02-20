#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <sys/stat.h>
#include <wasmtime.h>
#include "runner_stub.h"

#define MAGIC "PUPYPACK"
#define MAX_DIRS 64

static void exit_with_error(const char *msg, wasmtime_error_t *error) {
    fprintf(stderr, "error: %s\n", msg);
    if (error) {
        wasm_byte_vec_t emsg;
        wasmtime_error_message(error, &emsg);
        fprintf(stderr, "%.*s\n", (int)emsg.size, emsg.data);
        wasm_byte_vec_delete(&emsg);
        wasmtime_error_delete(error);
    }
    exit(1);
}

static void write_le64(FILE *f, uint64_t val) {
    uint8_t buf[8];
    for (int i = 0; i < 8; i++)
        buf[i] = (val >> (i * 8)) & 0xff;
    fwrite(buf, 1, 8, f);
}

static void usage(const char *prog) {
    fprintf(stderr, "Usage: %s <input.wasm> [-o <output>] [--dir=<path>] ...\n",
            prog);
    exit(1);
}

int main(int argc, char *argv[]) {
    if (argc < 2) usage(argv[0]);

    const char *input = NULL;
    const char *output = NULL;
    const char *dirs[MAX_DIRS];
    int num_dirs = 0;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-o") == 0) {
            if (++i >= argc) usage(argv[0]);
            output = argv[i];
        } else if (strncmp(argv[i], "--dir=", 6) == 0) {
            if (num_dirs >= MAX_DIRS) {
                fprintf(stderr, "error: too many --dir entries\n");
                exit(1);
            }
            dirs[num_dirs++] = argv[i] + 6;
        } else if (argv[i][0] == '-') {
            usage(argv[0]);
        } else {
            if (input) usage(argv[0]);
            input = argv[i];
        }
    }

    if (!input) usage(argv[0]);

    // Default output: strip .wasm extension or append .packed
    char default_output[1024];
    if (!output) {
        size_t len = strlen(input);
        if (len > 5 && strcmp(input + len - 5, ".wasm") == 0) {
            snprintf(default_output, sizeof(default_output), "%.*s",
                     (int)(len - 5), input);
        } else {
            snprintf(default_output, sizeof(default_output), "%s.packed",
                     input);
        }
        output = default_output;
    }

    // Read input .wasm
    FILE *fin = fopen(input, "rb");
    if (!fin) {
        fprintf(stderr, "error: cannot open %s\n", input);
        exit(1);
    }
    fseek(fin, 0, SEEK_END);
    long wasm_len = ftell(fin);
    fseek(fin, 0, SEEK_SET);
    uint8_t *wasm = malloc(wasm_len);
    if (!wasm || fread(wasm, 1, wasm_len, fin) != (size_t)wasm_len) {
        fprintf(stderr, "error: cannot read %s\n", input);
        exit(1);
    }
    fclose(fin);

    // Detect component vs core module (byte 4: 0x0d = component, 0x01 = core)
    uint8_t is_component = 0;
    if (wasm_len >= 8 && wasm[4] == 0x0d)
        is_component = 1;

    // Compile .wasm to native code
    // Engine config must match the minimal runner's capabilities
    wasm_config_t *config = wasm_config_new();
    wasmtime_config_wasm_gc_set(config, true);
    wasmtime_config_wasm_threads_set(config, false);
    if (is_component)
        wasmtime_config_wasm_component_model_set(config, true);
    wasm_engine_t *engine = wasm_engine_new_with_config(config);

    wasm_byte_vec_t cwasm;
    if (is_component) {
        wasmtime_component_t *component;
        wasmtime_error_t *error =
            wasmtime_component_new(engine, wasm, wasm_len, &component);
        if (error)
            exit_with_error("failed to compile component", error);
        free(wasm);

        error = wasmtime_component_serialize(component, &cwasm);
        if (error)
            exit_with_error("failed to serialize component", error);
        wasmtime_component_delete(component);
    } else {
        wasmtime_module_t *module;
        wasmtime_error_t *error =
            wasmtime_module_new(engine, wasm, wasm_len, &module);
        if (error)
            exit_with_error("failed to compile module", error);
        free(wasm);

        error = wasmtime_module_serialize(module, &cwasm);
        if (error)
            exit_with_error("failed to serialize module", error);
        wasmtime_module_delete(module);
    }
    wasm_engine_delete(engine);

    // Build config section
    // Format: u32 count, then pairs of NUL-terminated strings
    size_t config_cap = 4;
    for (int i = 0; i < num_dirs; i++)
        config_cap += strlen(dirs[i]) + 1 + strlen(dirs[i]) + 1;

    uint8_t *config_data = malloc(config_cap);
    uint32_t count = num_dirs;
    memcpy(config_data, &count, 4);
    size_t config_len = 4;
    for (int i = 0; i < num_dirs; i++) {
        size_t slen = strlen(dirs[i]) + 1;
        // host_path = guest_path = dirs[i]
        memcpy(config_data + config_len, dirs[i], slen);
        config_len += slen;
        memcpy(config_data + config_len, dirs[i], slen);
        config_len += slen;
    }

    // Write output: stub + cwasm + config + trailer
    FILE *fout = fopen(output, "wb");
    if (!fout) {
        fprintf(stderr, "error: cannot open %s for writing\n", output);
        exit(1);
    }

    fwrite(runner_stub, 1, runner_stub_len, fout);
    fwrite(cwasm.data, 1, cwasm.size, fout);
    fwrite(config_data, 1, config_len, fout);
    write_le64(fout, cwasm.size);
    write_le64(fout, config_len);
    fwrite(&is_component, 1, 1, fout);
    fwrite(MAGIC, 1, 8, fout);
    fclose(fout);

    chmod(output, 0755);

    size_t cwasm_size = cwasm.size;
    wasm_byte_vec_delete(&cwasm);
    free(config_data);

    printf("packed %s -> %s (%zu bytes)\n", input, output,
           runner_stub_len + cwasm_size + config_len + 25);
    return 0;
}
