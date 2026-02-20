#!/bin/sh
# Wrapper: presents the same CLI as native puppyc but runs puppyc.wasm via wasmtime
# Use --dir=/ so both absolute output paths (/tmp/...) and source paths work.
# Convert relative paths to absolute since --dir=/ maps from root.

args=""
next_action=none
for arg in "$@"; do
    case "$next_action" in
        skip)
            args="$args $arg"
            next_action=none
            continue ;;
        path)
            case "$arg" in
                /*) args="$args $arg" ;;
                *)  args="$args $(pwd)/$arg" ;;
            esac
            next_action=none
            continue ;;
    esac
    case "$arg" in
        --target)              args="$args $arg"; next_action=skip ;;
        -o|--wit|--lib-path)   args="$args $arg"; next_action=path ;;
        --*)                   args="$args $arg" ;;
        /*)                    args="$args $arg" ;;
        *)                     args="$args $(pwd)/$arg" ;;
    esac
done

exec wasmtime run --wasm gc --dir=/ puppyc.wasm --$args
