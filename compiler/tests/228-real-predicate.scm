;; expect: 1 1 1 0
(display (if (real? 42) 1 0))
(display " ")
(display (if (real? 1/3) 1 0))
(display " ")
(display (if (real? 3.14) 1 0))
(display " ")
(display (if (real? 3+4i) 1 0))
