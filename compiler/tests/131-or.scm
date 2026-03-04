;; expect: 1
;; expect: 42
;; expect: #f
(display (or 1 2 3))
(newline)
(display (or #f #f 42))
(newline)
(display (or))
