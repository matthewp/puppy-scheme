;; expect: #f
;; expect: #f
;; expect: #f
;; expect: #t
;; expect: #t
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
