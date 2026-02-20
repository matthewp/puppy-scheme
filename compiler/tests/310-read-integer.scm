
;; wasi-dir: tests
;; expect: 42 -7 100
(call-with-input-file "data/integers.txt"
  (lambda (port)
    (display (read port))
    (display " ")
    (display (read port))
    (display " ")
    (display (read port))
    (newline)))
