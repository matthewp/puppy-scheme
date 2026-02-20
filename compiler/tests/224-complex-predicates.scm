;; expect: 11110
(display (if (complex? 3+4i) 1 0))
(display (if (number? 3+4i) 1 0))
(display (if (eqv? 3+4i 3+4i) 1 0))
(display (if (= 3+4i 3+4i) 1 0))
(display (if (eqv? 3+4i 3+5i) 1 0))
