;; expect: 0
;; expect: 0
;; expect: 0
;; expect: 1
;; expect: 1
;; boolean? must handle non-i31 values without crashing
(display (boolean? "hello"))
(newline)
(display (boolean? '(1 2)))
(newline)
(display (boolean? (cons 1 2)))
(newline)
(display (boolean? #t))
(newline)
(display (boolean? #f))
