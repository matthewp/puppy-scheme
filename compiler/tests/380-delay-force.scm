;; expect: 34242ok
;; delay/force with memoization: thunk only runs once
(define p (delay (begin (display 3) 42)))
(display (force p))
(display (force p))
(display "ok")
