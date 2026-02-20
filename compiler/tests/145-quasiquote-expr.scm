;; expect: 42
(display (car (cdr `(1 ,(* 6 7) 3))))
