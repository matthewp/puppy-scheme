;;; io.scm — I/O runtime functions

(define rt-newline-wasi
  (list '()   ;; params
    '()   ;; locals
    (list
      '(%mem-store8 48 10)
      '(%stream-write (%get-stdout) 48 1 300)
      0)))

