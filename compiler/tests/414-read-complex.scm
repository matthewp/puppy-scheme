
;; wasi-dir: tests
;; expect: 1 3+4i 1 0-1i
(call-with-input-file "data/complex.txt"
  (lambda (port)
    (let ((a (read port)))
      (display (complex? a))
      (display " ")
      (display a)
      (display " "))
    (read port)  ;; skip -1-2i
    (read port)  ;; skip +4i
    (let ((d (read port)))
      (display (complex? d))
      (display " ")
      (display d)
      (newline))))
