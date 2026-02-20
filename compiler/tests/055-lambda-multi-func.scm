;; expect: 13
(define (double x) (* x 2))
(define (add1 x) (+ x 1))
(display (add1 (double 6)))
