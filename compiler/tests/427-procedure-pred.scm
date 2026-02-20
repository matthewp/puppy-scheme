;; expect: 1000
(display (procedure? (lambda (x) x)))
(display (procedure? 42))
(display (procedure? '()))
(display (procedure? "hello"))
(newline)
