# Scheme Interpreter / REPL

## Overview

A Scheme interpreter compiled by `puppyc.wasm`. It reuses the compiler's reader and expander, and adds a tree-walking evaluator. It runs under wasmtime as a REPL or script runner.

## Naming

Other Schemes use two patterns:

| Implementation | REPL/Interpreter | Compiler |
|---|---|---|
| Gambit | `gsi` | `gsc` |
| Chicken | `csi` | `csc` |
| Guile | `guile` | (integrated) |
| Chez | `scheme` / `petite` | (integrated) |
| Racket | `racket` | `raco` |

- **`puppyi` / `puppyc`** — follows Gambit/Chicken's `Xsi`/`Xsc` split. Consistent with the existing compiler binary name.
- **`puppy`** — follows Guile/Racket/Chez where the REPL is "the thing itself" and the compiler is the special-purpose tool.

TBD.

## Motivation

- Interactive development — explore and test Scheme code without compiling
- `eval` support — the interpreter *is* eval; it evaluates arbitrary Scheme at runtime
- Debugging aid — test language features interactively before relying on compiled output
- Completeness — most Scheme implementations ship both a compiler and an interpreter

## Architecture

```
stdin/file → Reader (read.scm) → Expander (expand.scm) → Evaluator (NEW) → print
```

The evaluator operates on the core AST produced by the expander: `lambda`, `if`, `set!`, `quote`, `begin`, `define`, and applications. All derived forms (let, cond, do, etc.) are already desugared before the evaluator sees them.

### Value Representation

The interpreter runs inside a compiled WASM module, so it uses the same WASM GC types as any compiled program — i31ref for fixnums, structs for pairs, etc. No separate value representation needed. A closure created by the evaluator is just a Scheme data structure:

```scheme
;; Interpreted closure: (params body env)
;; Distinguished from compiled procedures by a tag
(vector 'closure params body env)
```

Compiled builtins (car, cons, +, display, etc.) are real compiled functions. The interpreter dispatches to them by name.

### Environment

Environments are alist chains — simple, correct, no new data structures needed:

```scheme
;; env is a list of frames, each frame is an alist
;; ((((x . 1) (y . 2)) ((z . 3))) ...)
```

For `set!`, environment entries use a mutable box (a one-element vector) so mutation is visible through closures:

```scheme
;; Bind: (cons (cons name (vector value)) frame)
;; Lookup: (vector-ref box 0)
;; Set!: (vector-set! box 0 new-value)
```

## Core Evaluator

The evaluator is a recursive function over expanded forms:

```scheme
(define (eval-expr expr env)
  (cond
    ((symbol? expr) (env-lookup env expr))
    ((not (pair? expr)) expr)  ; self-evaluating: numbers, strings, booleans, chars
    ((eq? (car expr) 'quote) (cadr expr))
    ((eq? (car expr) 'if) ...)
    ((eq? (car expr) 'set!) ...)
    ((eq? (car expr) 'begin) ...)
    ((eq? (car expr) 'define) ...)
    ((eq? (car expr) 'lambda) (make-closure (cadr expr) (cddr expr) env))
    (else (eval-apply (car expr) (cdr expr) env))))
```

Application evaluates the operator and arguments, then either:
- Calls a compiled builtin directly
- Enters an interpreted closure (extend env, eval body)

## Builtin Dispatch

A dispatch table maps symbols to compiled procedures. Since `apply` is now available, dispatch is straightforward:

```scheme
(define (call-builtin proc args)
  (apply proc args))
```

## REPL

```scheme
(define (repl env)
  (display "> ")
  (let ((expr (read)))
    (if (eof-object? expr)
        (newline)
        (let ((expanded (expand-one expr ...)))
          (let ((result (eval-expr expanded env)))
            (unless (void? result)
              (write result)
              (newline))
            (repl env))))))
```

### Readline

WASI doesn't provide raw terminal access, so built-in readline is impractical. Instead, use `rlwrap`:

```
rlwrap wasmtime run --wasm gc --dir=. puppyi.wasm
```

The native `puppyi` binary (via puppypack) can invoke rlwrap automatically, or the shell alias / wrapper script can handle it.

### Script Mode

```
wasmtime run --wasm gc --dir=. puppyi.wasm -- script.scm
```

Read and evaluate all forms from the file, then exit. No prompt, no print (unless the script calls display/write).

## Prerequisites

Features the compiler needs before puppyi can be built:

### Required (done)

1. ~~**`apply`**~~ — ✅ Implemented. Runtime builtin with arity-dispatched `call_indirect`.

2. ~~**`procedure?`**~~ — ✅ Implemented. Inline type predicate via `ref.test` on TY-CLOSURE.

3. ~~**`vector->list`**~~ — ✅ Implemented. Runtime builtin, builds list right-to-left.

## Build Integration

```makefile
# Source files for the interpreter
INTERP_SOURCES = src/interpreter/main.scm src/interpreter/eval.scm

# Build puppyi.wasm using the self-hosted compiler
puppyi.wasm: puppyc.wasm $(INTERP_SOURCES) $(RUNTIME_SOURCES)
	wasmtime run --wasm gc --dir=/ puppyc.wasm -- --target wasi -o $@ src/interpreter/main.scm

# Native binary via puppypack
puppyi: puppyi.wasm puppypack
	./puppypack puppyi.wasm puppyi
```

## File Structure

```
src/interpreter/
  main.scm    — entry point, REPL loop, argument parsing
  eval.scm    — core evaluator + environment operations
  builtins.scm — builtin dispatch table
```

The interpreter includes the compiler's reader and expander via `(include "...")`.

## Phased Implementation

### Phase 1: Minimal evaluator

- Arithmetic, booleans, if, define, lambda, application
- Enough to run `(define (fib n) ...) (display (fib 10))`
- Script mode only (no REPL yet)

### Phase 2: Full language

- All builtins the compiler supports (pairs, strings, vectors, chars, I/O)
- REPL with prompt
- `define-syntax` / `syntax-rules` (handled by expander, should work for free)

### Phase 3: Beyond the compiler

- `eval` as a first-class procedure (trivial — call the evaluator)
- `interaction-environment` — return the current REPL environment
- `load` — read and evaluate a file

## What This Doesn't Cover

- **call/cc** — Still deferred. The interpreter could support escape continuations via exceptions, but full continuations need CPS or stack capture.
- **Debugger** — Future work. The interpreter's structure makes step-through debugging natural.
- **Performance** — Tree-walking is slow. Acceptable for interactive use and small scripts. Not a goal to compete with compiled output.
