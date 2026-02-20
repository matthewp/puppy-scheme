;; expect: ell
;; expect: hello
;; expect: 0
(display (substring "hello" 1 4))
(newline)
(display (substring "hello" 0 5))
(newline)
(display (string-length (substring "hello" 2 2)))
(newline)
