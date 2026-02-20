(include "html.scm")
(include "counter-view.scm")

(define code-snippet
  (string-append
    "(define count 0)\n"
    "\n"
    "(define (counter-view)\n"
    "  (html (div (@ (class \"counter\"))\n"
    "          (button (@ (on \"click\" \"on_decrement\")) \"-\")\n"
    "          (span (@ (class \"count\")) ,(number->string count))\n"
    "          (button (@ (on \"click\" \"on_increment\")) \"+\"))))\n"
    "\n"
    "(define (handle-event handler)\n"
    "  (cond ((equal? handler \"on_decrement\")\n"
    "         (if (> count 0)\n"
    "             (set! count (- count 1))))\n"
    "        ((equal? handler \"on_increment\")\n"
    "         (set! count (+ count 1)))))\n"))

(define css
  (string-append
    "*, *::before, *::after { margin: 0; padding: 0; box-sizing: border-box; } "
    "body { "
    "  font-family: 'Inter', system-ui, -apple-system, sans-serif;"
    "  background: #fff; color: #111;"
    "  min-height: 100vh;"
    "  display: flex; flex-direction: column;"
    "  align-items: center; padding-top: 6rem;"
    "  -webkit-font-smoothing: antialiased;"
    "} "
    "main { text-align: center; margin-bottom: 3rem; } "
    ".logo { width: 10rem; height: auto; margin: 1.5rem 0; } "
    "h1 {"
    "  font-size: 3.5rem; font-weight: 700;"
    "  letter-spacing: -0.05em; line-height: 1;"
    "  margin-bottom: 0.75rem;"
    "} "
    "p { font-size: 1.125rem; color: #555; font-weight: 400; } "
    ".examples { width: 100%; max-width: 40rem; padding: 0 1.5rem; } "
    "h2 {"
    "  font-size: 1.5rem; font-weight: 500;"
    "  letter-spacing: -0.03em;"
    "  margin-bottom: 1.5rem;"
    "} "
    "h3 {"
    "  font-size: 1.125rem; font-weight: 600;"
    "  letter-spacing: -0.02em;"
    "  margin-bottom: 0.5rem;"
    "} "
    ".examples-desc { font-size: 1rem; color: #555; margin-bottom: 1.5rem; } "
    ".example { display: flex; flex-direction: column; gap: 1.5rem; } "
    "pre {"
    "  background: #f6f6f6; border-radius: 0.5rem;"
    "  padding: 1.25rem; overflow-x: auto;"
    "} "
    "pre code {"
    "  font-family: 'SF Mono', 'Cascadia Code', 'Fira Code', monospace;"
    "  font-size: 0.875rem; line-height: 1.6;"
    "} "
    ".token.keyword { font-weight: 700; } "
    ".token.string { color: #555; } "
    ".token.comment { color: #999; font-style: italic; } "
    ".token.punctuation { color: #777; } "
    ".token.function { font-weight: 600; } "
    ".counter {"
    "  display: flex; align-items: center; gap: 2rem;"
    "} "
    ".counter button {"
    "  width: 3rem; height: 3rem;"
    "  border: 1.5px solid #111; background: transparent;"
    "  color: #111; font-size: 1.25rem;"
    "  font-family: inherit; cursor: pointer;"
    "  border-radius: 50%;"
    "  transition: background 0.15s ease, color 0.15s ease;"
    "} "
    ".counter button:hover { background: #111; color: #fff; } "
    ".count {"
    "  font-size: 2.5rem; font-weight: 600;"
    "  min-width: 4rem; text-align: center;"
    "  font-variant-numeric: tabular-nums;"
    "} "
    ".features {"
    "  display: grid; grid-template-columns: 1fr 1fr;"
    "  gap: 1rem; width: 100%; max-width: 40rem;"
    "  padding: 0 1.5rem; margin-bottom: 3rem;"
    "  text-align: left;"
    "} "
    ".feature {"
    "  padding: 1rem 1.25rem;"
    "  background: #f6f6f6; border-radius: 0.5rem;"
    "} "
    ".feature strong {"
    "  display: block; font-size: 0.9375rem;"
    "  font-weight: 600; margin-bottom: 0.25rem;"
    "} "
    ".feature span {"
    "  font-size: 0.875rem; color: #555; line-height: 1.4;"
    "} "
    ".usage { width: 100%; max-width: 40rem; padding: 0 1.5rem; margin-bottom: 3rem; } "
    ".usage p { font-size: 1rem; color: #555; margin-bottom: 1rem; } "
    ".usage pre { margin-bottom: 0.75rem; } "
    ".usage pre code { font-size: 0.8125rem; } "
    ".usage p code {"
    "  font-family: 'SF Mono', 'Cascadia Code', 'Fira Code', monospace;"
    "  font-size: 0.8125rem; background: #f6f6f6;"
    "  padding: 0.15em 0.4em; border-radius: 0.25rem;"
    "} "
    "footer {"
    "  margin-top: 4rem; width: 100%;"
    "  background: #111; color: #999;"
    "  padding: 2rem 1.5rem; text-align: center;"
    "  font-size: 0.875rem;"
    "} "
    "footer a { color: #fff; text-decoration: none; display: inline-flex; align-items: center; gap: 0.4rem; } "
    "footer a:hover { text-decoration: underline; }"))

(define github-icon
  "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 16 16\" width=\"16\" height=\"16\" fill=\"currentColor\"><path d=\"M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.58-8-8-8z\"/></svg>")

(define (page-html)
  (render-to-string
    (html (html
            (head
              (meta (@ (charset "utf-8")))
              (meta (@ (name "viewport") (content "width=device-width, initial-scale=1")))
              (title "Puppy Scheme")
              (link (@ (rel "icon") (type "image/png") (href "favicon.png")))
              (link (@ (rel "preconnect") (href "https://fonts.googleapis.com")))
              (link (@ (rel "preconnect") (href "https://fonts.gstatic.com")))
              (link (@ (rel "stylesheet") (href "https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap")))
              (style ,css))
            (body
              (main
                (img (@ (src "puppy-logo.webp") (alt "Puppy Scheme logo") (class "logo") (width "385") (height "279")))
                (h1 "Puppy Scheme")
                (p "A Scheme compiler targeting WebAssembly."))
              (section (@ (class "features"))
                (div (@ (class "feature"))
                  (strong "WASM GC")
                  (span "No runtime garbage collector &#8212; uses WebAssembly's built-in GC."))
                (div (@ (class "feature"))
                  (strong "Tiny output")
                  (span "Single-digit KB binaries. No bloated runtime linked in."))
                (div (@ (class "feature"))
                  (strong "Self-hosting")
                  (span "The compiler compiles itself to WebAssembly."))
                (div (@ (class "feature"))
                  (strong "R7RS libraries")
                  (span "define-library and import with dead code elimination.")))
              (section (@ (class "usage"))
                (h2 "Usage")
                (p "Compile a Scheme file to WebAssembly:")
                (pre (code "$ puppyc hello.scm -o hello.wasm\n$ wasmtime -W gc hello.wasm"))
                (p "Use a WIT file to define custom component exports:")
                (pre (code "$ puppyc app.scm --wit app.wit -o app.wasm"))
                (p "The compiler itself is a WASM module. "
                  (code "puppyc")
                  " is a thin wrapper around "
                  (code "puppyc.wasm")
                  ", which can be run directly by any WASI-compatible runtime:")
                (pre (code "$ wasmtime -W gc puppyc.wasm -- hello.scm -o hello.wasm")))
              (section (@ (class "examples"))
                (h2 "Examples")
                (h3 "Interactive counter")
                (p (@ (class "examples-desc")) "Compile Scheme to WebAssembly and build live components for the browser. This counter is server-rendered and hydrated on the client.")
                (div (@ (class "example"))
                  (div (@ (id "app"))
                    ,(render-to-string (counter-view)))
                  (pre (code (@ (class "language-scheme")) ,code-snippet))))
              (footer
                (a (@ (href "https://github.com/matthewp/puppy-scheme")) ,github-icon "GitHub"))
              (script (@ (type "module") (src "main.js")))
              (script (@ (src "https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/prism.min.js")))
              (script (@ (src "https://cdnjs.cloudflare.com/ajax/libs/prism/1.29.0/components/prism-scheme.min.js"))))))))

(define (render-page)
  (page-html))
