;; expect: 123
(define (foo) (values 1 2 3))
(let ((v (foo)))
  (display (car v))
  (display (car (cdr v)))
  (display (car (cdr (cdr v)))))
