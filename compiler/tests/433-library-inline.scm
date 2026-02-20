;; expect: 25
;; define-library in the program file itself
(define-library (mylib)
  (export square)
  (begin
    (define (square x) (* x x))))

(import (mylib))
(display (square 5))
