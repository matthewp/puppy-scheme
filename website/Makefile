.PHONY: build run clean

build: dist/homepage.wasm dist/counter.wasm dist/main.js dist/runtime.js dist/puppy-logo.webp dist/favicon.png

dist:
	mkdir -p dist

PUPPYC = wasmtime run --wasm gc --dir=/ ../compiler/puppyc.wasm --

dist/homepage.wasm: src/homepage.scm src/html.scm src/counter-view.scm src/page.wit | dist
	$(PUPPYC) --core --wit $(CURDIR)/src/page.wit -o $(CURDIR)/dist/homepage.wasm $(CURDIR)/src/homepage.scm

dist/counter.wasm: src/counter.scm src/opcodes.scm src/html.scm src/counter-view.scm src/counter.wit | dist
	$(PUPPYC) --core --wit $(CURDIR)/src/counter.wit -o $(CURDIR)/dist/counter.wasm $(CURDIR)/src/counter.scm

dist/main.js: src/main.js | dist
	cp src/main.js dist/main.js

dist/runtime.js: src/runtime.js | dist
	cp src/runtime.js dist/runtime.js

dist/puppy-logo.webp: src/puppy-logo.webp | dist
	cp src/puppy-logo.webp dist/puppy-logo.webp

dist/favicon.png: src/favicon.png | dist
	cp src/favicon.png dist/favicon.png

run: build
	npx wrangler dev

clean:
	rm -rf dist
