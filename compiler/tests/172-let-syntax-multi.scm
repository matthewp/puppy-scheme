;; expect: 7
(let-syntax ((add1 (syntax-rules ()
                     ((add1 x) (+ x 1))))
             (dbl  (syntax-rules ()
                     ((dbl x) (+ x x)))))
  (display (add1 (dbl 3))))
