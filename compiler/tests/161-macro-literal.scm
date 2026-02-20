;; expect: 10
(define-syntax my-check
  (syntax-rules (is)
    ((my-check x is y) (if (= x y) 1 0))))
(display (my-check 3 is 3))
(display (my-check 3 is 4))
