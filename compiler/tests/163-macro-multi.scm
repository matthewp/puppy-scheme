;; expect: 14299
(define-syntax my-and
  (syntax-rules ()
    ((my-and) 1)
    ((my-and x) x)
    ((my-and x y) (if x y 0))))
(display (my-and))
(display (my-and 42))
(display (my-and 1 99))
