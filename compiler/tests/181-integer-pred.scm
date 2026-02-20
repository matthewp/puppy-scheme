;; expect: 110
(display (if (integer? 0) 1 0))
(display (if (integer? 99) 1 0))
(display (if (integer? #\a) 1 0))
