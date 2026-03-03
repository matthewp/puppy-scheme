(define cli-snippet
  (string-append
    "(define args (cdr (command-line)))\n"
    "\n"
    "(if (null? args)\n"
    "    (display \"Hello, World!\")\n"
    "    (begin\n"
    "      (display \"Hello, \")\n"
    "      (display (car args))\n"
    "      (display \"!\")))\n"
    "(newline)\n"))

(define cli-output
  (string-append
    "$ puppyc hello.scm -o hello.wasm\n"
    "$ wasmtime -W gc hello.wasm\n"
    "Hello, World!\n"
    "$ wasmtime -W gc hello.wasm -- Puppy\n"
    "Hello, Puppy!\n"))

(define-component cli-example
  (render
    (html (div (@ (class "tab-panel"))
            (h3 "CLIs")
            (p "Compile Scheme to standalone WebAssembly binaries. "
               "Programs run on any WASI-compatible runtime with "
               "full access to stdin, stdout, files, and environment variables.")
            (pre (code (@ (class "language-scheme")) ,cli-snippet))
            (pre (code ,cli-output))))))
