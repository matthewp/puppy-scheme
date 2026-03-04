
;; wasi-dir: tests
;; expect: ... #t
(call-with-input-file "data/ellipsis.txt"
  (lambda (port)
    (let ((val (read port)))
      (display val)
      (display " ")
      (display (symbol? val))
      (newline))))
