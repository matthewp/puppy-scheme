;; expect: 42
(define-syntax when
  (syntax-rules ()
    ((when test body ...)
     (if test (begin body ...)))))
(define x 1)
(when #t (set! x 42))
(display x)
