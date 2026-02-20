;; expect: 11010
(display (if (eqv? 42 42) 1 0))
(display (if (eqv? #t #t) 1 0))
(display (if (eqv? 42 43) 1 0))
(display (if (eqv? #\a #\a) 1 0))
(display (if (eqv? #\a #\b) 1 0))
