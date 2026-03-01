# Puppy Scheme

A minimal self-hosting Scheme compiler targeting WebAssembly, using the WASM GC proposal for memory management.

The compiler is written in Scheme and compiles itself to `puppyc.wasm`. No custom garbage collector — all heap types use WASM GC. Output size is a first-class concern: a trivial program compiles to ~137 bytes.

## Quick Start

```bash
# Compile and run a program
./puppyc input.scm -o output.wasm
wasmtime run --wasm gc output.wasm

# Or use the WASM compiler directly
wasmtime run --wasm gc --dir=/ puppyc.wasm -- -o output.wasm input.scm
```

## Repository Structure

```
compiler/     — the Scheme-to-WASM compiler
website/      — project website (planned)
tools/        — JS tooling and utilities (planned)
```

## Local Development

### Requirements

- **wasmtime** — for running the compiler and compiled programs

### Optional

- **C compiler** — needed for puppypack builds
- **Rust toolchain + cmake** — for building puppypack (native binary packaging)

### Building and Testing

```bash
cd compiler
make test                        # run test suite (uses puppyc.wasm)
make test TEST=040-display       # run a single test
make puppyc.wasm                 # rebuild self-hosted WASM compiler
make clean
```

### puppypack

`puppypack` creates small, standalone native binaries from `.wasm` files. It precompiles the WASM to native code and bundles it with a minimal wasmtime runtime — no wasmtime installation needed to run the result.

```bash
cd compiler
make puppypack                   # build puppypack (requires wasmtime libs + Rust)
make puppyc                      # pack puppyc.wasm into a native binary
make install                     # install puppyc to /usr/local/bin
```

Building puppypack itself requires the wasmtime C API library and a one-time `make wasmtime-min` to build a minimal wasmtime runtime from source (needs Rust toolchain and cmake).

## Compiler Notes

- **Self-hosting** — the compiler compiles itself to WASM
- **WASM GC** for all heap types — no custom garbage collector
- **Dead code elimination** — unused builtins are not emitted
- **Proper tail calls** via WASM tail-call proposal (`return_call` / `return_call_indirect`)
- **`include`** for multi-file compilation
- **`define-library` / `import`** — R7RS module system

## R5RS Coverage

### Primitive Expressions (4.1)

- [x] Variable references
- [x] Literal expressions — integers, floats, rationals, booleans, strings
- [x] Character literals — `#\a`, `#\newline`
- [x] Procedure calls
- [x] `lambda`
- [x] `if`
- [x] `set!`
- [x] `quote` / `'`

### Derived Expressions (4.2)

- [x] `cond`
- [x] `case`
- [x] `and`
- [x] `or`
- [x] `let`
- [x] `let*`
- [x] `letrec`
- [x] `begin`
- [x] `do`
- [x] Named `let`
- [x] `when`
- [x] `unless`
- [ ] `delay` / `force`
- [x] Quasiquote / `` ` `` / `,` / `,@`

### Definitions (5)

- [x] `(define var expr)`
- [x] `(define (f x) body)`
- [x] Internal defines (body-level `define` → `letrec`)
- [x] `define-syntax`

### Macros (4.3)

- [x] `syntax-rules`
- [x] `let-syntax`
- [x] `letrec-syntax`

### Standard Procedures — Equivalence (6.1)

- [x] `eqv?`
- [x] `eq?`
- [x] `equal?`

### Standard Procedures — Numbers (6.2)

- [x] `+` `-` `*` `/`
- [x] `=` `<` `>` `<=` `>=`
- [x] `number?` `integer?` `exact?` `inexact?` `flonum?`
- [x] `zero?` `positive?` `negative?` `odd?` `even?`
- [x] `abs` `quotient` `remainder` `modulo`
- [x] `max` `min`
- [x] `gcd` `lcm`
- [x] `floor` `ceiling` `truncate` `round`
- [x] `exact->inexact` `inexact->exact`
- [x] `number->string` `string->number`
- [x] Inexact reals (flonums) — mixed fixnum/flonum arithmetic
- [x] Exact rationals — `1/3`, `(/ 1 3)` → `1/3`, GCD normalization
- [x] `rational?`
- [x] Complex numbers — `3+4i`, `make-rectangular`, `real-part`, `imag-part`, `complex?`
- [ ] `real?` `numerator` `denominator` `rationalize`
- [x] Transcendental functions — `sqrt` `exp` `log` `sin` `cos` `tan` `asin` `acos` `atan` `expt`

### Standard Procedures — Booleans (6.3.1)

- [x] `not`
- [x] `boolean?`
- [ ] `boolean=?`

### Standard Procedures — Pairs and Lists (6.3.2)

- [x] `pair?`
- [x] `cons` `car` `cdr`
- [x] `null?`
- [x] `set-car!` `set-cdr!`
- [x] `caar` `cadr` `cdar` `cddr` ... `cddddr`
- [x] `list?`
- [x] `list`
- [x] `length`
- [x] `append`
- [x] `reverse`
- [x] `list-tail`
- [x] `list-ref`
- [x] `memq` `memv` `member`
- [x] `assq`
- [x] `assv` `assoc`

### Standard Procedures — Symbols (6.3.3)

- [x] `symbol?`
- [x] `symbol->string`
- [x] `string->symbol`

### Standard Procedures — Characters (6.3.4)

- [x] `char?`
- [x] `char=?`
- [x] `char<?` `char>?` `char<=?` `char>=?`
- [x] `char-alphabetic?` `char-numeric?` `char-whitespace?`
- [x] `char->integer` `integer->char`
- [x] `char-upcase` `char-downcase`
- [x] Case-insensitive variants (`char-ci=?` ...)

### Standard Procedures — Strings (6.3.5)

- [x] `string?`
- [x] `make-string` `string-length` `string-ref` `string-set!`
- [x] `string`
- [x] `string=?`
- [x] `string<?` `string>?` `string<=?` `string>=?`
- [x] `substring` `string-copy`
- [x] `string-append`
- [x] `string->list` `list->string`
- [ ] `string-fill!`
- [x] Case-insensitive variants (`string-ci=?` ...)

### Standard Procedures — Vectors (6.3.6)

- [x] `vector?`
- [x] `make-vector` `vector` `vector-length` `vector-ref` `vector-set!`
- [x] `vector->list` `list->vector` `vector-copy`
- [ ] `vector-fill!`

### Standard Procedures — Control (6.4)

- [x] `procedure?`
- [x] `apply`
- [x] `map`
- [x] `for-each`
- [ ] `call-with-current-continuation`
- [x] `values` `call-with-values`
- [ ] `dynamic-wind`

### Standard Procedures — Eval (6.5)

- [ ] `eval`
- [ ] `scheme-report-environment` `null-environment`
- [ ] `load`

### Standard Procedures — I/O (6.6)

- [x] `display`
- [x] `newline`
- [x] `write-char` `write-bytevector`
- [x] `write`
- [x] `read-char` `peek-char`
- [x] `read`
- [x] `port?` `input-port?` `output-port?`
- [x] `file-exists?`
- [x] `open-input-file` `open-output-file`
- [x] `close-input-port` `close-output-port`
- [ ] `current-input-port` `current-output-port` `current-error-port`
- [x] `eof-object?`
- [ ] `eof-object`
- [ ] `char-ready?`
- [x] `call-with-input-file` `call-with-output-file`
- [ ] `with-input-from-file` `with-output-to-file`
- [x] `open-input-string` / [ ] `open-output-string` `get-output-string`
- [ ] `open-input-bytevector` `open-output-bytevector` `get-output-bytevector`
- [ ] `read-line` `read-string`
- [ ] `read-u8` `peek-u8` `read-bytevector` `read-bytevector!`
- [ ] `write-string` `write-u8`
- [ ] `input-port-open?` `output-port-open?`
- [ ] `flush-output-port`
- [ ] `textual-port?` `binary-port?`

## R7RS Coverage

### Program Structure (5.2)

- [x] `include`
- [x] `define-library` (`export`, `import`, `begin`, `include` declarations)
- [x] `import` — plain imports (no `only`, `except`, `rename`, `prefix` yet)

### Libraries (5.6)

- [x] `.sld` file resolution via search path
- [x] `--lib-path` CLI flag for additional search paths
- [x] Standard libraries (`(scheme base)`, `(scheme write)`, etc.) recognized as no-ops
- [x] Namespace isolation via symbol prefixing (internal names only)
- [x] Cycle detection and export conflict detection
- [ ] Import modifiers (`only`, `except`, `rename`, `prefix`)
- [ ] `cond-expand` in library declarations
- [ ] Macro export from libraries

### Derived Expressions

- [ ] `case-lambda`
- [ ] `let-values` `let*-values`
- [ ] `define-values`
- [ ] `guard`
- [ ] `define-record-type`
- [ ] `make-parameter` `parameterize`

### Exceptions (6.11)

- [ ] `error`
- [ ] `error-object?` `error-object-message` `error-object-irritants`
- [ ] `with-exception-handler` `raise` `raise-continuable`

### Additional Numbers

- [ ] `exact` `inexact`
- [ ] `exact-integer?`
- [ ] `finite?` `infinite?` `nan?`
- [ ] `square`
- [ ] `floor/` `floor-quotient` `floor-remainder`
- [ ] `truncate/` `truncate-quotient` `truncate-remainder`

### Additional Strings/Vectors

- [ ] `string-copy!`
- [ ] `vector-copy!`
- [ ] `string-map` `string-for-each`
- [ ] `vector-map` `vector-for-each`
- [ ] `list-copy`
- [ ] `make-list`

### Bytevectors (6.9)

- [x] `bytevector?`
- [x] `make-bytevector`
- [x] `bytevector-length`
- [x] `bytevector-u8-ref` `bytevector-u8-set!`
- [x] `bytevector-copy` `bytevector-copy!`
- [x] `bytevector-append`
- [x] `utf8->string` `string->utf8`

#### Bytevector extensions (not in R7RS)

- [x] `bytevector-u32-native-ref` `bytevector-u32-native-set!`
- [x] `bytevector->pointer`
- [x] `bytevector-copy-string!`
- [x] `pointer->string`

### Process Context (R7RS 6.14)

- [x] `command-line`
- [x] `exit`
- [x] `get-environment-variable`
- [x] `get-environment-variables`
- [x] `emergency-exit`

## WASI Preview 2 (Component Model)

All compiled programs output WASI P2 components. Only the interfaces a program actually uses are imported — a pure computation imports nothing beyond `wasi:cli/run`.

### Supported Interfaces

- [x] `wasi:cli/run@0.2.0` — program entry point (always present)
- [x] `wasi:cli/exit@0.2.0` — `exit`, `emergency-exit`
- [x] `wasi:io/streams@0.2.0` — stdout/stderr output, stdin input
- [x] `wasi:cli/stdout@0.2.0` — `display`, `write`, `newline`, `write-char`
- [x] `wasi:cli/stderr@0.2.0` — error output
- [x] `wasi:cli/stdin@0.2.0` — `read-char`, `peek-char`, `read`
- [x] `wasi:clocks/monotonic-clock@0.2.0` — `current-milliseconds`, `current-second`
- [x] `wasi:cli/environment@0.2.0` — `command-line`, `get-environment-variable`, `get-environment-variables`
- [x] `wasi:filesystem/types@0.2.0` — `open-input-file`, `open-output-file`, `close-input-port`, `close-output-port`, `file-exists?`
- [x] `wasi:filesystem/preopens@0.2.0` — preopened directory access for file I/O

### Not Yet Supported

- [ ] `wasi:sockets` — network access
- [ ] `wasi:random` — random number generation
- [ ] `wasi:clocks/wall-clock@0.2.0` — wall-clock time (monotonic clock is used instead)

## SRFI-1 (List Library) — partial

- [x] `take`
- [x] `filter`

## Puppy Extensions

- [x] `define-external` — export functions with concrete WASM type signatures for FFI
- [x] `linear-alloc` — allocate linear memory from the compiler's bump allocator: `(linear-alloc size align)` returns an aligned address as i31ref
