;; expect: 100
(display (if (equal? "hello" "hello") 1 0))
(display (if (equal? "hello" "world") 1 0))
(display (if (equal? "hi" "hello") 1 0))
