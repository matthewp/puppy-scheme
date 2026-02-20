;; expect: 10
(display (if (char? #\a) 1 0))
(display (if (char? 42) 0 0))
