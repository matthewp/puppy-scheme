PREFIX ?= /usr/local
CC = cc
CFLAGS = -std=c11 -D_GNU_SOURCE -Wall -Wextra -O2 -Isrc

PLATFORM_LIBS != if [ `uname -s` = Linux ]; then echo '-lpthread -ldl -lm'; else echo '-lpthread -lm'; fi

# Scheme source
SCM_MAIN = src/compiler/main.scm
SCM_MACRO = src/compiler/macro.scm
SCM_EXPAND = src/compiler/expand.scm
SCM_ANALYZE = src/compiler/analyze.scm
SCM_WASM = src/compiler/wasm.scm
SCM_CODEGEN_EXPR = src/compiler/codegen-expr.scm
SCM_CODEGEN = src/compiler/codegen.scm
SCM_LIBRARY = src/compiler/library.scm
SCM_RUNTIME != echo src/runtime/*.scm

test: puppyc.wasm
	@if [ -n "$(TEST)" ]; then tests/run_tests.sh ./tests/run_wasm.sh tests/$(TEST).scm; else tests/run_tests.sh ./tests/run_wasm.sh; fi

puppyc: puppypack puppyc.wasm
	./puppypack puppyc.wasm --dir=/ -o puppyc

install: puppyc
	install -d $(DESTDIR)$(PREFIX)/bin
	install -m 755 puppyc $(DESTDIR)$(PREFIX)/bin/puppyc

WASMTIME_DIR = native/wasmtime
WASMTIME_MIN_DIR = native/wasmtime/min
WASMTIME_VERSION = 41.0.3

native/runner: native/runner.c $(WASMTIME_DIR)/lib/libwasmtime.a
	$(CC) -std=c11 -Wall -Wextra -O2 -I$(WASMTIME_DIR)/include -o $@ $< $(WASMTIME_DIR)/lib/libwasmtime.a $(PLATFORM_LIBS)

# --- puppypack ---

# Build minimal wasmtime from source (runtime-only, no compiler)
WASMTIME_SRC = /tmp/wasmtime-build/wasmtime-$(WASMTIME_VERSION)
wasmtime-min:
	@echo "Downloading wasmtime v$(WASMTIME_VERSION) source..."
	mkdir -p /tmp/wasmtime-build
	curl -sL https://github.com/bytecodealliance/wasmtime/archive/refs/tags/v$(WASMTIME_VERSION).tar.gz \
		| tar xz -C /tmp/wasmtime-build
	@echo "Patching: make declare_vecs available without async feature..."
	sed -i '/^#\[cfg(feature = "async")\]/{N;/pub(crate) use declare_vecs/s/^#\[cfg(feature = "async")\]\n//;}' \
		$(WASMTIME_SRC)/crates/c-api/src/vec.rs
	@echo "Building minimal wasmtime (requires Rust toolchain)..."
	cd $(WASMTIME_SRC) && \
		cmake -S crates/c-api -B build \
			-DCMAKE_BUILD_TYPE=Release \
			-DWASMTIME_FEATURE_CRANELIFT=OFF \
			-DWASMTIME_FEATURE_GC=ON \
			-DWASMTIME_FEATURE_GC_DRC=ON \
			-DWASMTIME_FEATURE_WASI=ON \
			-DWASMTIME_FEATURE_COMPONENT_MODEL=ON \
			-DWASMTIME_FEATURE_ASYNC=OFF \
			-DWASMTIME_FEATURE_PARALLEL_COMPILATION=OFF \
			-DWASMTIME_FEATURE_CACHE=OFF \
			-DWASMTIME_FEATURE_PROFILING=OFF \
			-DWASMTIME_FEATURE_COREDUMP=OFF \
			-DWASMTIME_FEATURE_THREADS=OFF \
			-DWASMTIME_FEATURE_POOLING_ALLOCATOR=OFF \
			-DWASMTIME_FEATURE_WINCH=OFF \
			-DWASMTIME_FEATURE_PULLEY=OFF \
			-DWASMTIME_FEATURE_WAT=OFF \
			-DWASMTIME_FEATURE_DISABLE_LOGGING=ON \
			-DWASMTIME_FEATURE_DEMANGLE=OFF \
			-DWASMTIME_FEATURE_ADDR2LINE=OFF \
			-DWASMTIME_FEATURE_DEBUG_BUILTINS=OFF && \
		CARGO_PROFILE_RELEASE_OPT_LEVEL=s \
		CARGO_PROFILE_RELEASE_LTO=true \
		CARGO_PROFILE_RELEASE_CODEGEN_UNITS=1 \
		CARGO_PROFILE_RELEASE_STRIP=debuginfo \
		cmake --build build
	@echo "Installing to $(WASMTIME_MIN_DIR)..."
	cp $(WASMTIME_SRC)/target/*/release/libwasmtime.a $(WASMTIME_MIN_DIR)/lib/
	cp $(WASMTIME_SRC)/build/include/wasmtime/conf.h $(WASMTIME_MIN_DIR)/include/wasmtime/
	rm -rf /tmp/wasmtime-build
	@echo "Done. Minimal wasmtime installed to $(WASMTIME_MIN_DIR)"

# Runner stub: linked against minimal wasmtime (no compiler)
native/runner_stub: native/runner_precompiled.c $(WASMTIME_MIN_DIR)/lib/libwasmtime.a
	$(CC) -std=c11 -Wall -Wextra -O2 -s -I$(WASMTIME_MIN_DIR)/include -o $@ $< \
		$(WASMTIME_MIN_DIR)/lib/libwasmtime.a $(PLATFORM_LIBS)

# Convert runner stub to embeddable C header
native/runner_stub.h: native/runner_stub
	python3 -c "\
	import sys; d = open('native/runner_stub','rb').read(); \
	h = ','.join('0x%02x'%b for b in d); \
	f = open('native/runner_stub.h','w'); \
	f.write('unsigned char runner_stub[] = {%s};\n' % h); \
	f.write('unsigned int runner_stub_len = %d;\n' % len(d)); \
	f.close()"

# puppypack: links against full wasmtime (has compiler for .wasm -> .cwasm)
puppypack: native/puppypack.c native/runner_stub.h $(WASMTIME_DIR)/lib/libwasmtime.a
	$(CC) -std=c11 -Wall -Wextra -O2 -I$(WASMTIME_DIR)/include -Inative -o $@ $< \
		$(WASMTIME_DIR)/lib/libwasmtime.a $(PLATFORM_LIBS)

puppyc.wasm: $(SCM_MAIN) $(SCM_MACRO) $(SCM_EXPAND) $(SCM_ANALYZE) $(SCM_WASM) $(SCM_CODEGEN_EXPR) $(SCM_CODEGEN) $(SCM_LIBRARY) $(SCM_RUNTIME)
	wasmtime run --wasm gc --dir=/ $@ -- -o $(CURDIR)/$@.tmp $(CURDIR)/$(SCM_MAIN)
	mv $@.tmp $@

hello.wasm: puppyc.wasm hello.scm
	wasmtime run --wasm gc --dir=/ puppyc.wasm -- -o $(CURDIR)/$@ $(CURDIR)/hello.scm

hello: native/runner hello.wasm
	cp native/runner $@
	cat hello.wasm >> $@
	python3 -c "import struct,sys; sys.stdout.buffer.write(struct.pack('<Q', $$(stat -c%s hello.wasm)))" >> $@
	chmod +x $@

clean:
	rm -f native/runner hello.wasm hello puppyc.wasm puppyc
	rm -f native/runner_stub native/runner_stub.h puppypack puppyc-packed

.PHONY: test clean install wasmtime-min
