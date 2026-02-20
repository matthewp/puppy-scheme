;; expect: 42
(letrec-syntax ((double (syntax-rules ()
                          ((double x) (+ x x)))))
  (display (double 21)))
