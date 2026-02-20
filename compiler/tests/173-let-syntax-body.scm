;; expect: 12
(let-syntax ((k (syntax-rules ()
                  ((k x) x))))
  (display (k 1))
  (display (k 2)))
