(define-library (puppy utils)
  (export add1)
  (begin
    (define (helper x) (+ x 1))
    (define (add1 x) (helper x))))
