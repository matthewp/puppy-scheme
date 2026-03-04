;; expect: #t#f#f
;; promise? predicate
(display (promise? (delay 1)))
(display (promise? 42))
(display (promise? '()))
