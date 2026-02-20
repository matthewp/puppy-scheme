;; expect: 7
(define (add1 x) (+ x 1))
(define (twice f x) (f (f x)))
(display (twice add1 5))
