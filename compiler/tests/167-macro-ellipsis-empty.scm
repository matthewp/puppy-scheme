;; expect: 03
(define-syntax my-and
  (syntax-rules ()
    ((my-and) #t)
    ((my-and x) x)
    ((my-and x rest ...) (if x (my-and rest ...) #f))))
(display (if (my-and) 0 9))
(display (my-and 3))
