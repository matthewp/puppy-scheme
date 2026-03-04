;; expect: #t#f#t#f
(display (string? "hello"))
(display (string? 42))
(display (string? ""))
(display (string? #t))
(newline)
