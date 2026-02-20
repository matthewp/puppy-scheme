# Remove `--target`, Unify on Component Model

## Problem

The compiler had two targets (`--target wasi|web`) with divergent codegen paths:

- **WASI**: P1 core module with `wasi_snapshot_preview1` imports, or P2 component with stream-based I/O
- **Web**: Core module with `env.display` import + generated `.js` companion file

This created a three-way branch at every I/O site (P2 stream-write / P1 fd_write / web display), two command-line runtime functions, two environment-variable runtime functions, two newline functions, and a JS companion generator. The web target had no tests.

## Current State

Phases 1 and 2 are complete. The compiler defaults to Component Model output. The web target is gone. Exit is gated behind DCE. P1 remains only for features that don't have P2 equivalents yet (file-io, command-line, clock, get-env).

```
puppyc hello.scm -o hello.wasm          # component (auto-detected wasi:cli imports)
puppyc --wit api.wit app.scm -o app.wasm # component (custom WIT world)
puppyc --target wasi app.scm             # P1 core module (hidden flag, for self-hosting)
```

## What's Done

### Phase 1: Remove web target, default to component — DONE

- Deleted `TARGET_WEB` constant and all `(= target TARGET_WEB)` branches
- Deleted `write-js-companion`, `derive-js-path`, `%web-display` codegen op
- Deleted web runtime functions: `rt-newline-web`, `rt-command-line-web`, `rt-get-env-var-web`, `rt-get-env-vars-web`
- Removed `target` parameter from `make-rt-display`, `make-rt-write`
- Changed default target from web to WASI
- Expanded `is-p2` condition: programs without P1-only features (file-io, command-line, clock, get-env) auto-wrap as P2 components — including pure computation programs with no I/O
- Made `wrap-wasi-component` conditional on `needs-io`: pure computation components have zero WASI imports (except the `wasi:cli/run` export)
- Deleted 18 tests (3 web, 15 bare exit-code tests superseded by display-based tests)
- Removed `target: wasi` annotation from ~200 tests
- Hidden `--target` from `--help` output

### Phase 2: Gate exit behind DCE — DONE

- Added `FLAG-EXIT` (27) with scan entries for `"exit"` and `"emergency-exit"`
- `wasi:cli/exit` only imported when program calls exit
- `wrap-wasi-component` accepts `needs-exit` flag; conditional type/import/alias/canon-lower sections
- Component binary indices use running counters instead of hardcoded values
- All 266 tests pass, self-hosting fixed point verified

## What Remains

### Phase 3: P2 equivalents for P1-only features

Four features still require P1 core module output because their P2 WASI interfaces aren't implemented in the component wrapper. Each one needs: (a) component type/import sections in `component.scm`, (b) canon-lower entries to produce core functions, (c) updated codegen to call the lowered functions instead of P1 imports.

#### 3a. Clock — `wasi:clocks/wall-clock`

Simplest. One function, primitive types, no string marshalling.

P1 import: `clock_time_get(id: i32, precision: i64, timestamp_ptr: i32) -> i32`
P2 interface: `wasi:clocks/wall-clock` exports `now() -> record { seconds: u64, nanoseconds: u32 }`

The P2 function returns two values via the canonical ABI (lowered to a retptr since >1 flat result). The core function signature becomes `(i32) -> ()` where the i32 is a pointer to write `{seconds: u64, nanos: u32}` into linear memory.

Changes:
- `component.scm`: Add `wasi:clocks/wall-clock` type/import section (conditional on `needs-clock`)
- `codegen.scm`: P2 clock import emits `"clock-now"` from `"env"` instead of `"clock_time_get"` from `"wasi_snapshot_preview1"`
- `system.scm`: Update `rt-current-second` to read the retptr struct instead of the P1 timestamp format
- `analyze.scm`: Fold `needs-clock` into `is-p2` (remove from exclusion list)

#### 3b. Stdin — `wasi:cli/stdin` + `wasi:io/streams`

Needed for `read-char`, `peek-char`, and the `read` procedure when reading from stdin. Follows the same pattern as stdout/stderr (already implemented).

P1 import: `fd_read(fd: i32, iovs: i32, iovs_len: i32, nread_ptr: i32) -> i32`
P2 interface: `wasi:cli/stdin` exports `get-stdin() -> input-stream`, `wasi:io/streams` has `[method]input-stream.blocking-read(len: u64) -> result<list<u8>, stream-error>`

Changes:
- `component.scm`: Add `wasi:cli/stdin` type/import section, alias `get-stdin` function, canon-lower it. The `wasi:io/streams` import already exists when `needs-io` is true — extend it with `input-stream` type and `blocking-read` method
- `codegen.scm`: P2 read path calls lowered `get-stdin` + `blocking-read` instead of `fd_read(0, ...)`
- `io.scm` / `read.scm`: Update to use the new calling convention
- New flag: `FLAG-STDIN` or fold into existing `FLAG-FILE-IO`

#### 3c. Command-line args — `wasi:cli/environment`

Needs string+list ABI. Prerequisite: `cabi_realloc` export from the core module.

P1 imports: `args_sizes_get(argc_ptr, argv_buf_size_ptr) -> i32` + `args_get(argv_ptr, argv_buf_ptr) -> i32`
P2 interface: `wasi:cli/environment` exports `get-arguments() -> list<string>`

The lowered core function signature: `(i32) -> ()` where the i32 is a retptr. The host calls `cabi_realloc` to allocate space for the list and each string, then writes `(ptr, len)` pairs for each string element and the outer list `(ptr, count)` at the retptr.

Changes:
- `component.scm`: Add `wasi:cli/environment` type/import section with `get-arguments` function
- `codegen.scm`: Export `cabi_realloc` from core module (bump allocator already exists in `system.scm`, just needs the export entry)
- `system.scm`: Rewrite `rt-command-line` to call lowered `get-arguments` and parse the returned `list<string>` from linear memory (iterate the `(ptr, count)` array, read each `(ptr, len)` string, copy to GC string)
- `analyze.scm`: Remove `needs-command-line` from `is-p2` exclusion

#### 3d. Environment variables — `wasi:cli/environment`

Same interface as command-line args. Same prerequisites.

P1 imports: `environ_sizes_get` + `environ_get`
P2 interface: `wasi:cli/environment` exports `get-environment() -> list<tuple<string, string>>`

Changes:
- `component.scm`: Share the `wasi:cli/environment` instance with command-line (if both needed). Add `get-environment` function type
- `system.scm`: Rewrite `rt-get-env-var` and `rt-get-env-vars` to call lowered `get-environment` and parse the `list<tuple<string, string>>` from linear memory
- `analyze.scm`: Remove `needs-get-env` from `is-p2` exclusion

#### 3e. File I/O — `wasi:filesystem/types` + `wasi:filesystem/preopens`

Biggest lift. Requires resource handles, multiple methods, and string marshalling.

P1 imports: `path_open`, `fd_read`, `fd_close`
P2 interface: `wasi:filesystem/types` exports resource `descriptor` with methods `open-at`, `read-via-stream`, `write-via-stream`, `stat`. `wasi:filesystem/preopens` exports `get-directories() -> list<tuple<descriptor, string>>`

Resource handles are i32 values in the core ABI. The component model tracks ownership — `own<descriptor>` transfers ownership, `borrow<descriptor>` is a temporary reference. At the core level they're both i32.

Changes:
- `component.scm`: Add `wasi:filesystem/types` and `wasi:filesystem/preopens` type/import sections. These are large — `descriptor` has many methods. We only need `open-at`, `read-via-stream`, `write-via-stream`, `stat`, and the resource drop
- `codegen.scm`: Export `cabi_realloc` (shared with command-line)
- `file-io.scm`: Rewrite `open-input-file` / `open-output-file` to call lowered `get-directories` + `open-at`. Rewrite port read/write to use stream resources
- `file-exists.scm`: Rewrite to use `stat` or `open-at` with error handling
- `analyze.scm`: Remove `needs-file-io` from `is-p2` exclusion

#### Implementation order

1. **Clock** — no string marshalling, one function, validates the pattern
2. **cabi_realloc export** — prerequisite for everything with strings/lists
3. **Command-line** — first use of string+list ABI, moderate complexity
4. **Env vars** — same interface as command-line, incremental
5. **Stdin** — extends existing I/O streams pattern
6. **File I/O** — resource handles, biggest change, do last

After each step, the `is-p2` exclusion list shrinks. When all five are done, `is-p2` is always true (for non-`--target wasi` builds) and can be removed.

### Phase 4: Remove P1 entirely

Once all WASI interfaces have P2 equivalents, P1 code can be deleted. This is a large cleanup but mechanically straightforward — every P1 code path has a P2 equivalent by this point.

#### What gets deleted

| File | Deletions |
|---|---|
| `analyze.scm` | `is-p2` flag and all `is-p2` branches; `IDX-IS-P2`; P1 import index allocation (`IDX-FN-IO`, `IDX-FN-ARGS-SIZES-GET`, `IDX-FN-ARGS-GET`, `IDX-FN-PATH-OPEN`, `IDX-FN-FD-READ`, `IDX-FN-FD-CLOSE`, `IDX-FN-CLOCK-TIME-GET`, `IDX-FN-ENVIRON-SIZES-GET`, `IDX-FN-ENVIRON-GET`); P1 type index allocation for iovec/path_open/fd_close signatures |
| `codegen.scm` | P1 import section branch; P1 type signatures (`TY-IO-IMPORT` iovec-based, `ty-path-open`, `ty-fd-close`, `ty-clock-import`, `ty-args-import`); P1 newline/display/write runtime selection branches; `wasi_snapshot_preview1` string literals |
| `main.scm` | `--target` argument parsing; `TARGET_WASI` constant; the `is-p2` conditional in the wrapping logic (everything is always wrapped) |
| `display.scm` | P1 `fd_write`-based display path (iovec construction, nwritten pointer) |
| `write.scm` | P1 `fd_write`-based write path |
| `io.scm` | P1 `fd_write`-based newline; iovec helpers |
| `system.scm` | P1 `args_sizes_get`/`args_get` command-line implementation; P1 `environ_sizes_get`/`environ_get` env var implementation; P1 `clock_time_get` clock implementation |
| `file-io.scm` | P1 `path_open`/`fd_read`/`fd_close` implementations |
| `file-exists.scm` | P1 `path_open`-based file existence check |
| `component.scm` | Memory module (`build-cabi-realloc-module`) moves into the core module itself since `cabi_realloc` is now always a core module export |
| `Makefile` | Remove `--target wasi` from `puppyc.wasm` build rule |
| `tests/run_tests.sh` | Remove `target` variable and `--target` handling |
| `tests/run_wasm.sh` | Remove `--target` skip action |
| `tests/*.scm` | Remove remaining `target: wasi` annotations from P1-dependent tests |

#### What changes structurally

- The `target` parameter disappears from `analyze-forms`, `codegen-module`, and all runtime function generators
- `compute-index-map` has one path instead of P1/P2 branches
- Import section codegen has one path (the current P2 path, without the `is-p2` guard)
- `wrap-wasi-component` is always called (the `cond` in `main.scm` simplifies to just WIT-world vs built-in-world)
- The memory module currently built by `build-cabi-realloc-module` in `component.scm` becomes unnecessary — the core module exports its own memory and `cabi_realloc` directly. The component instantiates one core module instead of two

#### Self-hosting consideration

The compiler itself uses file-io, command-line, and file-exists. All of these must have working P2 equivalents before P1 can be removed. The self-hosting build (`make puppyc.wasm`) is the final validation — if the compiler can compile itself as a component and the result works, P1 is fully replaced.

Verification: after removing P1, `make puppyc.wasm` builds without `--target wasi`, produces a component binary, and `make test` passes with all 266+ tests running against the component-output compiler.

## Size Impact

Every output is now a component binary. The component envelope adds ~250-800 bytes depending on which WASI interfaces are imported:

| Program | Core module | Component |
|---|---|---|
| `(+ 1 2)` | ~100 bytes | ~373 bytes |
| `(display 42)` | ~300 bytes | ~1100 bytes |
| `(display "hello")(newline)` | ~400 bytes | ~1200 bytes |
| Self-hosting compiler | ~508 KB | ~508 KB |

For non-trivial programs the overhead is negligible. For trivial programs, <1.5 KB is still tiny.

## Browser / Web Usage

Without the old `--target web` and its `.js` companion, browser users have two options:

### Option 1: Use `jco` (recommended)

[jco](https://github.com/nicolo-ribaudo/jco) transpiles Component Model binaries to JS+WASM:

```sh
npx jco transpile hello.wasm -o hello-js/
```

This generates a JS wrapper that provides all WASI imports using browser APIs.

### Option 2: Manual instantiation

Extract the core module from the component and provide imports directly:

```js
const memory = new WebAssembly.Memory({ initial: 1 });
const { instance } = await WebAssembly.instantiateStreaming(
  fetch("hello-core.wasm"),
  {
    mem: { memory },
    env: {
      "get-stdout": () => 1,
      "get-stderr": () => 2,
      "stream-write": (stream, ptr, len, ret_ptr) => {
        const bytes = new Uint8Array(memory.buffer, ptr, len);
        console.log(new TextDecoder().decode(bytes));
      }
    }
  }
);
instance.exports.run();
```

### Option 3: Future — browser-native Component Model

The [Component Model for the web](https://github.com/nicolo-ribaudo/component-model-cpp-wasm-api) effort aims to add native component support to browsers. When that ships, `.wasm` component files will just work with no tooling.
