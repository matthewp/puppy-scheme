;; expect: 3
;; expect: 0
;; expect: 1
(display (and 1 2 3))
(newline)
(display (and 1 #f 3))
(newline)
(display (and))
