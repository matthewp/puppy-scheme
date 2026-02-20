;; expect: 142
(define-syntax my-let
  (syntax-rules ()
    ((my-let ((var init) ...) body ...)
     ((lambda (var ...) body ...) init ...))))
(display (my-let ((x 1)) x))
(display (my-let ((x 40)) (+ x 2)))
