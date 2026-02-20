;; expect: 15
(define (add-n n)
  (display ((lambda (x) (+ x n)) 10))
  (newline))
(add-n 5)
