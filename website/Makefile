.PHONY: build run clean

PUPPYKIT = ../puppykit/src
PUPPYKIT_DEPS = $(PUPPYKIT)/html.scm $(PUPPYKIT)/component.scm $(PUPPYKIT)/opcodes.scm $(PUPPYKIT)/puppykit/ui.sld

build: dist/homepage.wasm dist/app.wasm dist/main.js dist/puppykit.js dist/puppy-logo.webp dist/favicon.png

dist:
	mkdir -p dist

PUPPYC = wasmtime run --wasm gc --dir=/ ../compiler/puppyc.wasm --

dist/homepage.wasm: src/homepage.scm $(PUPPYKIT_DEPS) src/app/hello.scm src/app/counter.scm src/app/tabs.scm src/page.wit | dist
	$(PUPPYC) --core --wit $(CURDIR)/src/page.wit --lib-path $(CURDIR)/$(PUPPYKIT) -o $(CURDIR)/dist/homepage.wasm $(CURDIR)/src/homepage.scm

dist/app.wasm: src/app.scm $(PUPPYKIT_DEPS) src/app/hello.scm src/app/counter.scm src/app/tabs.scm ../puppykit/app.wit | dist
	$(PUPPYC) --core --wit $(CURDIR)/../puppykit/app.wit --lib-path $(CURDIR)/$(PUPPYKIT) -o $(CURDIR)/dist/app.wasm $(CURDIR)/src/app.scm

dist/main.js: src/main.js | dist
	cp src/main.js dist/main.js

dist/puppykit.js: ../puppykit/puppykit.js | dist
	cp ../puppykit/puppykit.js dist/puppykit.js

dist/puppy-logo.webp: src/puppy-logo.webp | dist
	cp src/puppy-logo.webp dist/puppy-logo.webp

dist/favicon.png: src/favicon.png | dist
	cp src/favicon.png dist/favicon.png

run: build
	npx wrangler dev

clean:
	rm -rf dist
