;; expect: #t
;; expect: #t
;; expect: #f
;; expect: #f
;; expect: #t
(display (boolean=? #t #t))
(newline)
(display (boolean=? #f #f))
(newline)
(display (boolean=? #t #f))
(newline)
(display (boolean=? #f #t))
(newline)
(display (boolean=? #t #t #t))
