# Puppy Scheme

A minimal Scheme compiler targeting WebAssembly, with plans for a website and JS tooling.

## Monorepo Structure

```
compiler/     — the Scheme-to-WASM compiler (see compiler/CLAUDE.md)
website/      — project website (future)
tools/        — JS tooling and utilities (future)
```

## Project Goals

- **Compile Scheme to WASM** with minimal (ideally zero) runtime overhead
- **Leverage WASM GC** for memory management — no custom garbage collector
- **Start with R5RS subset**, expand incrementally
- **Eventually self-hosting** — the compiler should compile itself
- **Future type system** — architecture should stay flexible for adding types later
- **Small output size** — WASM binaries must be small enough for fast browser loads. A trivial program should produce a tiny module. No dead code, no bloated runtime, no kitchen-sink stdlib linked in. Think single-digit KB for simple programs.
- **Minimal philosophy** — Gambit-inspired: lean output, small footprint, no bloat

## Workflow

- Never commit without being explicitly asked to

## Code Style

- Keep files small and focused — one concern per file
- No premature abstraction — inline until repetition demands otherwise
- Minimal comments — only where the "why" isn't obvious from the code
