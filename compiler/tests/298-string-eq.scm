;; expect: #t#f#t#f
(display (string=? "hello" "hello"))
(display (string=? "hello" "world"))
(display (string=? "" ""))
(display (string=? "hi" "h"))
(newline)
