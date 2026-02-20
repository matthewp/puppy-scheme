;; expect: 100100
(display (if (positive? 5) 1 0))
(display (if (positive? 0) 1 0))
(display (if (positive? -3) 1 0))
(display (if (negative? -3) 1 0))
(display (if (negative? 0) 1 0))
(display (if (negative? 5) 1 0))
