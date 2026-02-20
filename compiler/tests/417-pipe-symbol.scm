;; Test: pipe-delimited symbols have pipes stripped per R5RS/R7RS
;; |foo:bar| should produce a symbol named "foo:bar", not "|foo:bar|"

;; wasi-dir: tests
;; expect: hello:world foo bar
(call-with-input-file "data/pipe-symbol.txt"
  (lambda (port)
    (display (symbol->string (read port)))
    (display " ")
    (display (symbol->string (read port)))
    (newline)))
