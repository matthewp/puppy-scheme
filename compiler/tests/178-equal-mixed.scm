;; expect: 110
(display (if (equal? '() '()) 1 0))
(display (if (equal? #\a #\a) 1 0))
(display (if (equal? 42 "hello") 1 0))
