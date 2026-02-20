
;; wasi-dir: tests
;; expect: ... 1
(call-with-input-file "data/ellipsis.txt"
  (lambda (port)
    (let ((val (read port)))
      (display val)
      (display " ")
      (display (symbol? val))
      (newline))))
