;; expect: 42
(let-syntax ((double (syntax-rules ()
                       ((double x) (+ x x)))))
  (display (double 21)))
