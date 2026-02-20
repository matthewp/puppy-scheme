;; expect: 2
;; expect: 3
;; expect: 4
(display (cadr '(1 2 3)))
(newline)
(display (caddr '(1 2 3 4)))
(newline)
(display (cadddr '(1 2 3 4)))
(newline)
