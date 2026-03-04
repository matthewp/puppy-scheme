.PHONY: build run clean

build: dist/homepage.wasm dist/app.wasm dist/main.js dist/runtime.js dist/puppy-logo.webp dist/favicon.png

dist:
	mkdir -p dist

PUPPYC = wasmtime run --wasm gc --dir=/ ../compiler/puppyc.wasm --

dist/homepage.wasm: src/homepage.scm src/html.scm src/component.scm src/app/ui.sld src/app/hello.scm src/app/counter.scm src/app/tabs.scm src/page.wit | dist
	$(PUPPYC) --core --wit $(CURDIR)/src/page.wit -o $(CURDIR)/dist/homepage.wasm $(CURDIR)/src/homepage.scm

dist/app.wasm: src/app.scm src/app.wit src/html.scm src/component.scm src/app/ui.sld src/app/hello.scm src/app/counter.scm src/app/tabs.scm | dist
	$(PUPPYC) --core --wit $(CURDIR)/src/app.wit -o $(CURDIR)/dist/app.wasm $(CURDIR)/src/app.scm

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
