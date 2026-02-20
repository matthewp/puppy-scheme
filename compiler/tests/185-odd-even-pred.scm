;; expect: 101011
(display (if (odd? 3) 1 0))
(display (if (odd? 4) 1 0))
(display (if (even? 4) 1 0))
(display (if (even? 3) 1 0))
(display (if (odd? -1) 1 0))
(display (if (even? 0) 1 0))
