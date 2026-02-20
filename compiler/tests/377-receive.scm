;; expect: 123
(define (foo) (values 1 2 3))
(receive (a b c) (foo)
  (display a)
  (display b)
  (display c))
