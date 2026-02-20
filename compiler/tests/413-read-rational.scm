
;; wasi-dir: tests
;; expect: 1 1/3
(call-with-input-file "data/rationals.txt"
  (lambda (port)
    (let ((a (read port)))
      (display (number? a))
      (display " ")
      (display a)
      (newline))))
