# Component Model and WASI Preview 2

## Context

WASI Preview 2 (0.2) is stable (released January 2024, now at 0.2.10). It replaces the POSIX-like P1 API (`wasi_snapshot_preview1`) with the WebAssembly Component Model — structured interfaces defined in WIT, typed values, resources with automatic cleanup.

Puppy currently targets WASI P1 with raw function imports (`fd_write`, `proc_exit`, etc.) from a single flat namespace. This works but P1 is effectively frozen — all new WASI development happens on P2/P3.

## Goals

1. **Replace WASI P1 with direct Component Model (P2) emission.** Drop P1 entirely — no adapter shim, no backward compatibility layer. The `--target wasi` flag produces a component binary.

2. **General Component Model support.** Users can define custom WIT worlds and the compiler produces components that implement them. WASI is just one world among many.

## Current P1 Surface (to be replaced)

Imports from `wasi_snapshot_preview1`:

| Import | Used by |
|---|---|
| `proc_exit` | exit, emergency-exit (P1 only; P2 uses `wasi:cli/exit`) |
| `fd_write` | display, write, newline, error output |
| `fd_read` | read-char, peek-char |
| `path_open` | open-input-file, open-output-file, file-exists? |
| `fd_close` | close-port |
| `args_sizes_get` + `args_get` | command-line |
| `environ_sizes_get` + `environ_get` | get-environment-variable(s) |
| `clock_time_get` | current-second |

All gated by DCE flags — only imported when the program actually uses them.

## Component Model Basics

A component wraps a core WASM module with typed imports/exports. The binary format uses the same `\0asm` magic but version 13 + layer 1 (vs version 1 + layer 0 for core modules).

Structure of a minimal component:

```wat
(component
  (core module $core
    ;; The actual program
    (memory (export "memory") 1)
    (func (export "cabi_realloc") (param i32 i32 i32 i32) (result i32) ...)
    (func (export "run") ...)
  )
  (core instance $inst (instantiate $core))

  ;; Lift core export to component-level typed function
  (func $run (result (result))
    (canon lift (core func $inst "run")))
  (export "run" (func $run))
)
```

Key requirements from the canonical ABI:
- **`memory`** — exported linear memory for string/list marshalling
- **`cabi_realloc`** — `(i32 i32 i32 i32) -> i32` allocator for the runtime to write incoming data
- **Flat ABI** — up to 16 i32/i64/f32/f64 params, 1 result; spills to memory via pointers beyond that
- Strings stored as `(ptr: i32, byte_length: i32)` in linear memory (UTF-8 by default)
- Lists stored as `(ptr: i32, count: i32)` with contiguous elements

### P2 equivalents of our P1 imports

| P1 | P2 interface | P2 function |
|---|---|---|
| `fd_write(1, ...)` | `wasi:cli/stdout` + `wasi:io/streams` | `get-stdout() -> output-stream`, then `blocking-write-and-flush(data: list<u8>)` |
| `fd_write(2, ...)` | `wasi:cli/stderr` + `wasi:io/streams` | `get-stderr() -> output-stream`, then `blocking-write-and-flush(data: list<u8>)` |
| `fd_read` | `wasi:io/streams` | `blocking-read(len: u64) -> list<u8>` |
| `proc_exit` | `wasi:cli/exit` | `exit(status: result)` |
| `args_*` | `wasi:cli/environment` | `get-arguments() -> list<string>` |
| `environ_*` | `wasi:cli/environment` | `get-environment() -> list<tuple<string, string>>` |
| `path_open` | `wasi:filesystem/types` | descriptor resource methods |
| `fd_close` | implicit | resource drop |
| `clock_time_get` | `wasi:clocks/wall-clock` | `now() -> datetime` |

## GC and Linear Memory

Puppy stores all data in GC-managed types (i31ref, GC structs, GC arrays). Both P1 and P2 pass data through linear memory — this isn't a new problem introduced by the Component Model. We already marshal between GC and linear memory for every WASI P1 call: `fd_write` takes iovec pointers, `args_get` writes into memory buffers, `path_open` takes a string pointer, etc. The runtime functions in `display.scm`, `system.scm`, `file-io.scm`, and `file-exists.scm` handle this today.

P2's canonical ABI is actually slightly simpler in some cases — strings are `(ptr, len)` directly rather than requiring iovec indirection.

The Component Model adds one new requirement: **`cabi_realloc`**, an exported allocator the runtime calls to write incoming data (e.g., argument strings, environment variables) into our linear memory. This is a simple bump allocator over a reserved memory region.

There is a [pre-proposal](https://github.com/WebAssembly/component-model/issues/525) to add GC support to the canonical ABI (strings as `(ref (array i8))`, lists as `(ref (array T))`) which would eliminate marshalling entirely. No timeline, but it's the ideal end state.

## What Changes in Codegen

1. **Binary preamble:** Component header instead of module header.

2. **Component sections:** The core module goes into section 1 (core module). Then we emit section 2 (core instance), section 7 (component types), section 8 (canonical definitions), section 10 (imports), section 11 (exports).

3. **Import lowering:** Instead of importing `wasi_snapshot_preview1::fd_write` directly into the core module, the component imports the typed P2 interface and `canon lower`s it into a core function the module can call. The core module's imports become internal linkage, not external.

4. **Export lifting:** The module's `_start` gets lifted to a component-level `run: func() -> result` export via `canon lift`.

5. **`cabi_realloc`:** Must be exported from the core module. This is a simple bump allocator over a reserved region of linear memory — separate from the GC heap.

6. **Linear memory:** Required even though we use GC for everything else. Used only for WASI marshalling — string buffers, list buffers, iovec-like structures.

### Marshalling functions

These already exist in spirit (the P1 runtime copies strings to/from memory). For P2 they get slightly cleaner because P2 uses `(ptr, len)` pairs directly rather than iovecs:

```
write-string-to-memory(gc-string) -> (ptr, len)
read-string-from-memory(ptr, len) -> gc-string
write-bytes-to-memory(gc-bytevector) -> (ptr, len)
read-bytes-from-memory(ptr, len) -> gc-bytevector
```

### Concrete example: display

P1 (current):
```
display(value):
  format value to GC string
  copy string bytes to linear memory at offset
  build iovec at another offset (ptr + len)
  call fd_write(1, iovec_ptr, 1, nwritten_ptr)
```

P2 (direct):
```
display(value):
  format value to GC string
  copy string bytes to linear memory at offset
  call lowered-blocking-write-and-flush(ptr, len)
```

The difference is small — P2 is actually simpler because it doesn't need the iovec indirection.

### Component binary layout

```
[component header: \0asm 0D000100]
[section 10: imports]
  import "wasi:cli/stdout@0.2.0" (instance ...)
  import "wasi:cli/stderr@0.2.0" (instance ...)
  import "wasi:cli/exit@0.2.0" (instance ...)
  import "wasi:io/streams@0.2.0" (instance ...)
  ...conditional based on DCE flags...
[section 7: types]
  component function types for imports/exports
[section 8: canon lower]
  lower each imported interface function to a core func
[section 1: core module]
  the actual program (what we emit today minus the P1 import section)
  plus: memory export, cabi_realloc export
[section 2: core instance]
  instantiate core module with lowered imports
[section 8: canon lift]
  lift "run" export
[section 11: exports]
  export "run" as component function
```

## Custom WIT Worlds

The Component Model isn't just for WASI — any host can define a WIT world, and components that target that world can run anywhere the world is implemented. This is how plugin systems, microservices, and embedded scripting work in the component ecosystem.

### How it works in Rust (for reference)

Rust uses a separate tool (`wit-bindgen` or `cargo-component`) that reads the WIT and generates `.rs` files — structs for records, traits for exported interfaces, wrapper functions for imports. This is a separate code generation step that runs *before* `rustc`, because Rust's type checker needs to see the generated types before it can compile user code that references them.

### Why Puppy doesn't need bindgen

Scheme is dynamically typed. There's no type checker that needs to see a declaration of `log` before you call `(log "hello")`. The compiler can read the WIT at compile time, see that `log` is an import taking a string, and generate the marshalling glue internally. No intermediate `.scm` files, no codegen step, no generated code to check in.

The WIT file is the only declaration. The compiler reads it and handles everything.

### How it works in Puppy Scheme

A `--wit` flag pointing to a WIT file (or directory):

```bash
puppyc --wit widget.wit -o widget.wasm my-widget.scm
```

Given this WIT:

```wit
package my:widgets@1.0.0;

interface renderer {
  record point { x: f32, y: f32 }
  render: func(label: string, pos: point) -> bool;
}

world widget {
  import log: func(msg: string);
  export renderer;
}
```

The user writes plain Scheme:

```scheme
(define (render label pos)
  (log (string-append "rendering: " label))
  #t)
```

The compiler:

1. **Parses the WIT** and extracts the world's imports and exports.

2. **Resolves imports by name.** When the compiler sees a call to `log` and it's not defined in the Scheme source, it checks the WIT imports. If there's a match, it emits a lowered call with the right marshalling. If there's no match in either Scheme or WIT, it's a compile error as usual.

3. **Resolves exports by name.** The WIT world expects `render` to be exported. The compiler checks that a top-level `(define render ...)` exists with the right arity. If it's missing, compile error: "world expects export `render` but no definition found."

4. **Maps WIT types to Scheme types:**

   | WIT type | Scheme type |
   |---|---|
   | `string` | string |
   | `bool` | boolean |
   | `u8, u16, u32, s8, s16, s32` | fixnum (i31ref) |
   | `u64, s64, f32, f64` | boxed number |
   | `list<T>` | list |
   | `tuple<T...>` | vector |
   | `record { ... }` | vector (fields by position) or alist |
   | `variant` / `enum` | symbol (for enums) or tagged pair |
   | `option<T>` | value or `#f` |
   | `result<T, E>` | value or error |
   | `flags` | integer (bitfield) |

5. **Emits the component wrapper** — `canon lower` for each import, `canon lift` for each export, with marshalling generated from the WIT types.

The same Scheme code works as a regular program or as a component export — the `--wit` flag is what makes the difference.

### WIT parsing

WIT is a simple text format. We need a parser that handles:
- Package declarations, interface definitions, world definitions
- Type definitions (record, variant, enum, flags, resource, type aliases)
- Function signatures with named parameters and results
- `use` statements for cross-interface type references

Written in Scheme, compiled into the compiler.

### WASI as a WIT world

With general WIT support, WASI becomes just a built-in world. Instead of hardcoding WASI imports in the compiler, we ship the WASI WIT definitions and the compiler reads them like any other world. The `--target wasi` flag is equivalent to `--wit wasi-cli.wit --world command`.

This is cleaner than what we have now (hardcoded `wasi_snapshot_preview1` import names in `codegen.scm`) and means new WASI versions or custom WASI subsets are just different WIT files.

### Scope: one world per component

A component implements exactly one world — this is a Component Model constraint. One `puppyc --wit` invocation produces one `.wasm` component. If you need multiple components, compile separately.

A single world can export multiple interfaces, so one component can still provide a large surface area. But there's no way to pack multiple independent worlds into one binary.

Component **composition** (wiring one component's exports to another's imports) is handled by external tools like `wasm-compose`. Note that composition duplicates code — if two Puppy components share library code, it's embedded twice. For shared code, prefer one world with multiple exported interfaces over separate composed components.

## Implementation Steps

### 1. WIT parser — DONE
New file: `src/compiler/wit.scm`. Parses `.wit` files into an internal representation — worlds with typed function imports and exports. Handles package declarations, world definitions, function signatures with named parameters and results, and primitive types (u8–u64, s8–s64, f32, f64, bool, char, string). The parser is written in Scheme and compiled into the compiler.

### 2. Component binary encoder — DONE
New file: `src/compiler/component.scm`. Wraps a core WASM module in the component binary format — component header (`\0asm` version 13, layer 1), component sections for core module (1), core instance (2), alias (6), types (7), canonical definitions (8), imports (10), exports (11). Output validates against `wasm-tools validate --features component-model`.

### 3. `cabi_realloc` export
Expose the existing `linear-alloc` bump allocator (`system.scm`) as a `cabi_realloc` export with the canonical ABI signature `(i32, i32, i32, i32) -> i32`. The bump pointer resets after each ABI crossing since the linear memory is scratch space — the runtime writes incoming data, our code copies it into GC types, and the buffer is dead. This is already how P1 works; we just need the export. Needed for string/list WIT types.

### 4. Import lowering — DONE
When `--wit` specifies a world with imports, the compiler creates synthetic user-func wrappers that unbox i31ref→i32 for params, call the lowered core import, and box i32→i31ref for the result. Unresolved Scheme identifiers are matched against WIT import names. WIT import wrappers are DCE roots so they survive dead code elimination. The component encoder emits `canon.lower` entries and wires the lowered imports into the core module instantiation. Tested with `tests/511-component-import.scm` (calculator world: imports `double`, exports `quadruple`).

### 5. Export lifting — DONE
When `--wit` specifies a world with exports, the compiler generates export shim functions that unbox i31ref→i32 for params, call the user function, and box i32→i31ref for the result. The component encoder emits `canon.lift` entries and component-level exports. Tested with `tests/510-component-add.scm` (exports `add`) — validates and executes correctly via `wasmtime run --invoke`.

### 6. WASI P2 as default output — DONE
The compiler now produces Component Model binaries by default. Programs without P1-only features (file-io, command-line, clock, get-env) are automatically wrapped as WASI P2 components via `wrap-wasi-component`. The component imports are DCE-driven: `wasi:cli/exit` only when `(exit)` is called, `wasi:io/streams` + `wasi:cli/stdout` + `wasi:cli/stderr` only when I/O is used. Pure computation programs produce components with zero WASI imports.

### 7. Web target removed — DONE
The `--target web` flag, all web-specific codegen (`%web-display`, `rt-newline-web`, `rt-command-line-web`, `rt-get-env-var-web`, `rt-get-env-vars-web`), the JS companion file generator, and the `TARGET_WEB` constant have been deleted. Browser users should use [jco](https://github.com/nicolo-ribaudo/jco) to transpile component binaries to JS+WASM.

### 8. P2 interfaces for remaining P1 features
See `design/target-refactor.md` for the plan to implement P2 equivalents of file-io, command-line, clock, and env — the features that still require P1 core module output.

### 9. Remove P1
Once all WASI interfaces have P2 equivalents, delete all `wasi_snapshot_preview1` import code, iovec construction, the old P1 calling conventions in the runtime files (`display.scm`, `system.scm`, `file-io.scm`, `file-exists.scm`), and the `--target` flag. See `design/target-refactor.md` Phase 4.

### 10. Update runner
Update `runner.c` / wasmtime C API integration to load components instead of core modules.

### Future: GC canonical ABI
When the GC canonical ABI proposal lands, we can eliminate the linear memory marshalling entirely — strings and lists pass as GC references across the component boundary. This is the ideal end state for Puppy but depends on spec work outside our control.

## WASI 0.3

WASI 0.3 (in release candidate as of February 2026) adds native async — `stream<T>` and `future<T>` as first-class WIT types. Not relevant for us yet since all our I/O is synchronous, but worth noting that the component binary format is forward-compatible.

## Runtime Support

| Runtime | P2 support | Notes |
|---|---|---|
| Wasmtime | Full (v17+) | Reference runtime, what we use |
| Wasmer | Partial | Not a priority |
| Browsers (via jco) | Transpile component to JS+core WASM |
| Node.js | via jco | Same transpile path as browsers |

Wasmtime is our primary target. Browser and Node.js support goes through `jco` (JavaScript Component Tools) which transpiles components to JS glue + core WASM.
