;; expect: #t#f#f#f
(display (procedure? (lambda (x) x)))
(display (procedure? 42))
(display (procedure? '()))
(display (procedure? "hello"))
(newline)
