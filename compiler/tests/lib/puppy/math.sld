(define-library (puppy math)
  (import (puppy core))
  (export double-add)
  (begin
    (define (double-add x y) (+ (double x) (double y)))))
