;; expect: 3
;; expect: #f
;; expect: #t
(display (and 1 2 3))
(newline)
(display (and 1 #f 3))
(newline)
(display (and))
