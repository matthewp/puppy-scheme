
;; wasi-dir: tests
;; expect: hello world +
(call-with-input-file "data/symbols.txt"
  (lambda (port)
    (display (read port))
    (display " ")
    (display (read port))
    (display " ")
    (display (read port))
    (newline)))
