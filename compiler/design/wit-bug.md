# Self-Hosting Bug: puppyc.wasm fails on --wit with larger programs

## Summary

`puppyc.wasm` (the WASM-compiled compiler) fails to compile programs using `--wit` when the source has ~1082+ total bytes of string literal data. The same input compiles successfully with `puppygc` (native Gambit build). Without `--wit`, `puppyc.wasm` compiles the same programs fine.

The error is:
```
error: WIT export 'render-page' not found in source
```

The WIT export function survives DCE in `puppygc` but gets incorrectly eliminated in `puppyc.wasm`.

## Reproduction

```
# Works:
puppygc --core --wit src/page.wit src/homepage.scm -o dist/homepage.wasm

# Fails:
puppyc --core --wit src/page.wit src/homepage.scm -o dist/homepage.wasm
```

A minimal reproduction: any program with `--wit` exporting a function, where total string literal content exceeds ~1082 characters. Below that threshold, `puppyc.wasm` works fine.

## What we know

- The function is present in the source and correctly defined (`(define (render-page) ...)`)
- All analysis phases complete (confirmed via `--profile` output)
- The function is missing from the post-DCE filtered function list when codegen runs
- `find-user-func` in `codegen.scm` does a linear `string=?` scan over the filtered list and doesn't find it
- The bug is size-dependent — not triggered by specific string content, just total volume
- Increasing WASM stack size doesn't help
- Hyphens in function names are not the cause (tested with `renderpage`)
- `define` vs `define-external` is not the cause

## Where to look

The DCE pipeline in `analyze.scm`:
1. `build-dep-graph` collects roots — WIT exports are added as roots at lines ~1175-1185
2. `mark-reachable` does worklist reachability from roots
3. Filtering (lines ~1612-1633) keeps functions where `(string-ht-has? reachable (uf-name uf))` is true

The string hash table (`string-hash`, `string-ht-set!`, `string-ht-has?`) uses a 64-bucket chained hash with `(bitwise-and (sum-of-char-codes) 63)`. Possible failure modes in WASM:
- Integer overflow in the hash sum behaving differently under WASM GC's i31ref arithmetic
- `string=?` comparison failing on strings that should match
- Hash table corruption from a GC-related issue with larger heap pressure
- The WIT export name not being added as a root at all (the `*wit-world*` global or its contents may be corrupt)

## Current workaround

The website Makefile uses `puppygc` instead of `puppyc` for WIT compilation.
