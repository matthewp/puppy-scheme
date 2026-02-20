.PHONY: build run clean

build: dist/homepage.wasm dist/counter.wasm dist/main.js dist/runtime.js dist/puppy-logo.webp dist/favicon.png

dist:
	mkdir -p dist

dist/homepage.wasm: src/homepage.scm src/html.scm src/counter-view.scm src/page.wit | dist
	../compiler/puppygc --core --wit src/page.wit src/homepage.scm -o dist/homepage.wasm

dist/counter.wasm: src/counter.scm src/opcodes.scm src/html.scm src/counter-view.scm src/counter.wit | dist
	../compiler/puppygc --core --wit src/counter.wit src/counter.scm -o dist/counter.wasm

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
