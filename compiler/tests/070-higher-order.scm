;; expect: 7
(define (add1 x) (+ x 1))
(define (apply-to f x) (f x))
(display (apply-to add1 6))
