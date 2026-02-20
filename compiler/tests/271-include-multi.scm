;; expect: 8
(include "lib/add3.scm")
(include "lib/add4.scm")
(display (+ (add3 1) (add4 0)))
(newline)
