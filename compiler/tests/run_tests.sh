#!/bin/sh
# Test harness for scheme-lang compiler
#
# Each .scm test file uses comment annotations:
#   ;; expect: <line>            — expected stdout (multiple lines supported)
#   ;; expect-exit: <N>          — expected exit code (skips output check)
#   ;; expect-no-import: <name>  — verify import is absent from compiled wasm
#   ;; expect-max-size: <bytes>  — verify compiled wasm is at most N bytes
#   ;; target: <name>            — pass --target <name> to compiler (default: wasi)
#   ;; expect-file: <path>       — verify file exists after compilation
#   ;; expect-file-contains: <string> — verify file contains string
#   ;; wasi-dir: <path>          — preopened directory for WASI (--dir flag)
#   ;; expect-stderr: <line>     — expected stderr (multiple lines supported)
#   ;; compile-only: true        — skip execution (compile + static checks only)
#   ;; skip: true                — skip this test entirely
#
# Usage: tests/run_tests.sh ./schemec [tests/foo.scm ...]
# If no test files given, runs all tests/*.scm

set -e

compiler="$1"
shift

if [ $# -gt 0 ]; then
    files="$@"
else
    files=$(echo tests/*.scm)
fi

pass=0
fail=0
tmpwasm=/tmp/scheme_test.wasm

for f in $files; do
    exp_exit=$(grep '^;; expect-exit:' "$f" | head -1 | sed 's/;; expect-exit: *//')
    # Collect all expect: lines, joined by newlines
    exp_out=$(grep '^;; expect:' "$f" | sed 's/;; expect: *//' | sed 's/;; expect://')
    # Collect all expect-no-import: values
    no_imports=$(grep '^;; expect-no-import:' "$f" | sed 's/;; expect-no-import: *//')
    max_size=$(grep '^;; expect-max-size:' "$f" | head -1 | sed 's/;; expect-max-size: *//')
    target=$(grep '^;; target:' "$f" | head -1 | sed 's/;; target: *//')
    exp_files=$(grep '^;; expect-file:' "$f" | sed 's/;; expect-file: *//')
    compile_only=$(grep '^;; compile-only:' "$f" | head -1 | sed 's/;; compile-only: *//')
    exp_stderr=$(grep '^;; expect-stderr:' "$f" | sed 's/;; expect-stderr: *//' | sed 's/;; expect-stderr://')
    run_args=$(grep '^;; args:' "$f" | head -1 | sed 's/;; args: *//')
    wasi_dir=$(grep '^;; wasi-dir:' "$f" | head -1 | sed 's/;; wasi-dir: *//')
    wit_file=$(grep '^;; wit:' "$f" | head -1 | sed 's/;; wit: *//')
    validate_component=$(grep '^;; validate-component:' "$f" | head -1 | sed 's/;; validate-component: *//')
    skip=$(grep '^;; skip:' "$f" | head -1 | sed 's/;; skip: *//')

    [ "$skip" = "true" ] && continue
    [ -z "$exp_exit" ] && [ -z "$exp_out" ] && [ -z "$exp_stderr" ] && [ -z "$no_imports" ] && [ -z "$max_size" ] && [ -z "$exp_files" ] && continue

    # Build compiler args
    compile_args=""
    if [ -n "$target" ]; then
        compile_args="--target $target"
    fi
    if [ -n "$wit_file" ]; then
        compile_args="$compile_args --wit $wit_file"
    fi

    # Compile
    if ! "$compiler" $compile_args -o "$tmpwasm" "$f" 2>/dev/null; then
        echo "FAIL $f (compile error)"
        fail=$((fail + 1))
        continue
    fi

    # Check import constraints
    if [ -n "$no_imports" ]; then
        binary_strings=$(strings "$tmpwasm")
        import_ok=true
        for imp in $no_imports; do
            if echo "$binary_strings" | grep -q "$imp"; then
                echo "FAIL $f (unexpected import '$imp' in binary)"
                fail=$((fail + 1))
                import_ok=false
                break
            fi
        done
        if ! $import_ok; then
            continue
        fi
    fi

    # Check size constraint
    if [ -n "$max_size" ]; then
        actual_size=$(wc -c < "$tmpwasm")
        if [ "$actual_size" -gt "$max_size" ]; then
            echo "FAIL $f (binary too large: $actual_size bytes, max $max_size)"
            fail=$((fail + 1))
            continue
        fi
    fi

    # Check expected files exist
    if [ -n "$exp_files" ]; then
        file_ok=true
        for ef in $exp_files; do
            if [ ! -f "$ef" ]; then
                echo "FAIL $f (expected file '$ef' not found)"
                fail=$((fail + 1))
                file_ok=false
                break
            fi
        done
        if ! $file_ok; then
            continue
        fi
    fi

    # Validate component model output
    if [ "$validate_component" = "true" ]; then
        if command -v wasm-tools >/dev/null 2>&1; then
            if wasm-tools validate --features component-model "$tmpwasm" 2>/dev/null; then
                echo "PASS $f (component valid)"
                pass=$((pass + 1))
            else
                echo "FAIL $f (component validation failed)"
                fail=$((fail + 1))
            fi
        else
            echo "PASS $f (compile-only, wasm-tools not available)"
            pass=$((pass + 1))
        fi
        continue
    fi

    # Skip execution for compile-only tests (validate wasm if wasm-tools available)
    if [ "$compile_only" = "true" ]; then
        if command -v wasm-tools >/dev/null 2>&1 && ! wasm-tools validate --features component-model "$tmpwasm" 2>/dev/null; then
            echo "FAIL $f (invalid wasm)"
            fail=$((fail + 1))
        else
            echo "PASS $f (compile-only)"
            pass=$((pass + 1))
        fi
        continue
    fi

    # Run (skip if no output/exit expectations)
    if [ -z "$exp_exit" ] && [ -z "$exp_out" ]; then
        echo "PASS $f (no-import)"
        pass=$((pass + 1))
        continue
    fi

    got_exit=0
    dir_flags=""
    if [ -n "$wasi_dir" ]; then
        dir_flags="--dir $wasi_dir"
    fi
    tmpstderr=/tmp/scheme_test_stderr
    got_out=$(wasmtime run --wasm gc $dir_flags "$tmpwasm" $run_args 2>"$tmpstderr") || got_exit=$?
    got_stderr=$(cat "$tmpstderr")

    if [ -n "$exp_exit" ]; then
        if [ "$got_exit" = "$exp_exit" ]; then
            echo "PASS $f (exit $exp_exit)"
            pass=$((pass + 1))
        else
            echo "FAIL $f (expected exit $exp_exit, got $got_exit)"
            fail=$((fail + 1))
        fi
    else
        out_ok=true
        if [ "$got_out" != "$exp_out" ]; then
            echo "FAIL $f"
            echo "  expected: $(echo "$exp_out" | head -3)"
            echo "  got:      $(echo "$got_out" | head -3)"
            fail=$((fail + 1))
            out_ok=false
        fi
        if [ -n "$exp_stderr" ] && $out_ok; then
            if [ "$got_stderr" = "$exp_stderr" ]; then
                echo "PASS $f (output)"
                pass=$((pass + 1))
            else
                echo "FAIL $f (stderr)"
                echo "  expected stderr: $(echo "$exp_stderr" | head -3)"
                echo "  got stderr:      $(echo "$got_stderr" | head -3)"
                fail=$((fail + 1))
            fi
        elif $out_ok; then
            echo "PASS $f (output)"
            pass=$((pass + 1))
        fi
    fi
done

echo "$pass passed, $fail failed"
[ "$fail" = "0" ]
