;; expect: 3
;; expect: aaa
(display (string-length (make-string 3)))
(newline)
(display (make-string 3 #\a))
(newline)
