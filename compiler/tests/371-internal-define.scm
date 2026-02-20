;; expect: 10
(define (foo x)
  (define (double n) (+ n n))
  (double x))
(display (foo 5))
