# Puppy Scheme Compiler

The self-hosting Scheme-to-WASM compiler.

## Architecture

**Pipeline:**
1. **Reader** — Tokenizer + S-expression parser
2. **Expander** — Macro expansion and desugaring (e.g., `let` → `lambda`, `cond` → `if`)
3. **Core AST** — Small core language: `lambda`, `if`, `set!`, `quote`, application, variables
4. **Codegen** — Emit WASM using the GC proposal's type system

### WASM Target

- **Primary:** WASI (via wasmtime/wasmer) for development and testing
- **Secondary:** Browser (via JS interop) — design should not preclude this
- **WASM GC proposal** for all heap-allocated Scheme values (pairs, closures, strings, vectors)
- No custom allocator or GC runtime shipped in the module
- **Output size is a first-class concern.** Only emit types, functions, and imports that the program actually uses. Dead code elimination at the WASM level — if a program doesn't use pairs, don't emit the pair struct type. A `(display 42)` program should compile to hundreds of bytes, not kilobytes.

### Value Representation (WASM GC types)

- **Fixnums** — `i31ref` (unboxed 31-bit integers)
- **Pairs** — `(struct (ref eq) (ref eq))` (car, cdr)
- **Closures** — `(struct (ref $code) (ref $env))` where env is an array
- **Strings** — WasmGC arrays of bytes (or i8)
- **Vectors** — WasmGC arrays of `(ref eq)`
- **Symbols** — Interned strings (index into a symbol table)
- **Booleans** — Distinguished `i31ref` values
- **Nil/void** — Distinguished `i31ref` values

### What We're NOT Doing (Yet)

- **call/cc** — Deferred. No CPS transform or stack manipulation for now.
- **Tail call optimization via WASM tail-call proposal** — Use when available, trampoline as fallback.
- **Modules/libraries** — Single-file compilation first.
- **Type system** — Future work. Keep the core AST extensible for type annotations.

## Language Coverage

See the root `README.md` for the full R5RS/R7RS coverage checklist.

## Dead Code Elimination

The compiler uses reachability-based DCE: after lambda lifting, it builds a dependency graph over all definitions (user functions, globals, builtins) and does mark-sweep from roots (top-level expressions). Only reachable functions, imports, and types are emitted. Unreachable user-defined functions are also eliminated.

All WASM index computation is centralized in `analyze.scm` via `compute-index-map`. Codegen reads indices from the analysis result rather than recomputing them. Builtin-to-builtin dependencies are declared as data in `*builtin-deps*`.

When adding new builtins: add the builtin name to `*scan-ht*`, add a FLAG constant, add dependencies to `*builtin-deps*`, and add index entries to `compute-index-map`. See `design/dead-code-elimination.md` for details.

## Bootstrapping & Self-Hosting

`puppyc.wasm` compiles itself. When making compiler changes:

- **Always keep a working `puppyc.wasm` copy** before modifying source. Copy it (e.g., `cp puppyc.wasm puppyc-backup.wasm`) before starting any bootstrap cycle. If gen1 or gen2 fails, you can recover from the backup instead of needing the native `./puppyc`.
- **Bootstrap cycle:** baseline → gen1 (baseline compiles new source) → gen2 (gen1 compiles itself). Gen2 should be stable (gen3 == gen2 in size).
- **Never overwrite `puppyc.wasm` without a backup** during development. The Makefile's `make puppyc.wasm` overwrites in-place.
- **The native `./puppyc` binary** is the last-resort bootstrap if `puppyc.wasm` gets corrupted, but it's an older version and may not match current source features.
- **Two separate `TY_FIXED_COUNT` definitions** exist: `TY-FIXED-COUNT` in `wasm.scm` and `TY_FIXED_COUNT` in `analyze.scm`. They MUST always match.

## Code Style

- C code: C11, no external dependencies beyond libc
- Keep files small and focused — one concern per file
- No premature abstraction — inline until repetition demands otherwise
- Minimal comments — only where the "why" isn't obvious from the code

## Directory Structure

```
src/compiler/
  main.scm        — entry point, argument parsing, read loop
  expand.scm      — macro expansion and desugaring
  macro.scm       — syntax-rules pattern matching
  analyze.scm     — scope analysis, reachability DCE, index computation
  codegen.scm     — module-level WASM codegen
  codegen-expr.scm — expression-level WASM codegen
  wasm.scm        — WASM binary format helpers
src/runtime/
  *.scm           — runtime support linked into compiled programs
tests/
  *.scm           — test programs with expected output
native/
  puppypack.c     — native binary packager
  runner.c        — WASI runtime runner
Makefile
```
