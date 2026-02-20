;; expect: 110
(display (if (equal? '(1 2 3) '(1 2 3)) 1 0))
(display (if (equal? '(1 (2 3)) '(1 (2 3))) 1 0))
(display (if (equal? '(1 2) '(1 3)) 1 0))
