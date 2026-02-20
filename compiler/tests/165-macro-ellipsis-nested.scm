;; expect: 3
(define-syntax my-let
  (syntax-rules ()
    ((my-let ((var init) ...) body ...)
     ((lambda (var ...) body ...) init ...))))
(display (my-let ((x 1) (y 2)) (+ x y)))
