;; expect: 42
(let-syntax ((when (syntax-rules ()
                     ((when test body ...)
                      (if test (begin body ...))))))
  (let ((x 1))
    (when #t (set! x 42))
    (display x)))
