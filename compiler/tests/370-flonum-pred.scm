;; expect: 1010
(display (if (flonum? 1.5) 1 0))
(display (if (flonum? 42) 0 0))
(display (if (flonum? 0.0) 1 0))
(display (if (flonum? #t) 0 0))
