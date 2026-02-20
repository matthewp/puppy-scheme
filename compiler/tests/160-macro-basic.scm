;; expect: 1
(define-syntax my-if
  (syntax-rules ()
    ((my-if test then else)
     (if test then else))))
(display (my-if #t 1 2))
