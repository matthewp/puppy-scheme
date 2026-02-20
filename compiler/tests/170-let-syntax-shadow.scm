;; expect: 220
(define-syntax add-n
  (syntax-rules ()
    ((add-n x) (+ x 1))))
(display (add-n 1))
(let-syntax ((add-n (syntax-rules ()
                      ((add-n x) (+ x 10)))))
  (display (add-n 10)))
