;; expect: #t#t#f#f
;; eq? on symbols must work — same-name symbols must be eq?
(display (eq? 'foo 'foo))
(display (eq? 'bar 'bar))
(display (eq? 'foo 'bar))
(display (eq? 'foo 42))
