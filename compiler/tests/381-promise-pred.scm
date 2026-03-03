;; expect: 100
;; promise? predicate
(display (promise? (delay 1)))
(display (promise? 42))
(display (promise? '()))
