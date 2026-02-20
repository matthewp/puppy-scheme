# Bootstrap: Removing the Gambit Dependency

## Problem

The self-hosted compiler (`puppyc.wasm`) is built by `puppygc` (Gambit-compiled). This means building from source requires Gambit installed, even though the WASM compiler is now the primary compiler.

## Goal

Remove Gambit from the default build path. A developer should only need `wasmtime` to build and develop puppy-scheme.

## Staged Bootstrap

Self-hosting compilers have a circular dependency: you need the compiler to build the compiler. The standard solution is staged bootstrapping.

- **Stage 0** — a pre-existing `puppyc.wasm` (downloaded or already installed)
- **Stage 1** — built by stage 0: `puppyc.wasm(stage0)` compiles sources → `puppyc-stage1.wasm`
- **Stage 2** — built by stage 1: `puppyc-stage1.wasm` compiles sources → `puppyc-stage2.wasm`

If the compiler is correct, stage 1 and stage 2 produce identical output. This is the "fixed-point" check — it proves the compiler can faithfully reproduce itself.

## Acquiring Stage 0

Two options, decide when releases exist:

### Option A: Require prior version installed

Like Go (before 1.20) and GHC. The Makefile checks for `puppyc` or `puppyc.wasm` on `$PATH` and uses it as stage 0.

- Pro: no network dependency, no download infrastructure
- Con: chicken-and-egg for first-time users

### Option B: Download from release

Like Rust's `rustup` or Go 1.20+. A script downloads a known `puppyc.wasm` from a GitHub release.

- Download `puppyc.wasm` directly (not the packed `puppyc` binary) — this way the only local dependency is `wasmtime`, which is already required for development
- Pin the bootstrap version (e.g. in a `BOOTSTRAP_VERSION` file)
- Cache the download so it's a one-time cost

- Pro: zero-friction onboarding, fully reproducible
- Con: network dependency, needs release infrastructure

Option B is probably better once releases exist. Option A works as a stopgap.

## Makefile Targets

```makefile
# Stage 0: bootstrap compiler (downloaded or pre-installed)
BOOTSTRAP_WASM = bootstrap/puppyc.wasm

$(BOOTSTRAP_WASM):
	./scripts/fetch-bootstrap.sh

# Stage 1: compile with stage 0
puppyc-stage1.wasm: $(BOOTSTRAP_WASM) $(SCM_SOURCES)
	wasmtime run --wasm gc --dir=/ $(BOOTSTRAP_WASM) -- --target wasi -o $@ $(SCM_MAIN)

# Stage 2: compile with stage 1 (should match stage 1)
puppyc-stage2.wasm: puppyc-stage1.wasm $(SCM_SOURCES)
	wasmtime run --wasm gc --dir=/ $< -- --target wasi -o $@ $(SCM_MAIN)

# Verify and promote
puppyc.wasm: puppyc-stage1.wasm
	cp $< $@

# Full verification
bootstrap-verify: puppyc-stage1.wasm puppyc-stage2.wasm
	diff puppyc-stage1.wasm puppyc-stage2.wasm
	@echo "Bootstrap verified: stage1 == stage2"
```

## When to Update the Bootstrap Compiler

The bootstrap `puppyc.wasm` only needs updating when the compiler's own language requirements change — e.g. the compiler starts using a feature that the old bootstrap compiler can't compile. This should be rare. Normal development (adding features to compile *user* code) doesn't require updating the bootstrap.

## Gambit as Fallback

Keep the `puppygc` target for:

- Bootstrapping from scratch when no `puppyc.wasm` exists yet
- Debugging compiler issues where you need to compare Gambit vs self-hosted output
- CI verification that both paths produce the same compiler

## Status

Not yet implemented. Gambit is still required for building `puppyc.wasm`. Implement this when release infrastructure is in place.
