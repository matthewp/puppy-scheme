;; expect: 15
(define (make-adder n)
  (lambda (x) (+ x n)))
(define add5 (make-adder 5))
(display (add5 10))
