;; expect: 1100
(display (if (number? 42) 1 0))
(display (if (number? -7) 1 0))
(display (if (number? "hello") 1 0))
(display (if (number? '(1 2)) 1 0))
