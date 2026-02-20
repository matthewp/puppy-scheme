
;; expect: hello1
(let ((p (open-input-string "hello")))
  (display (read-char p))
  (display (read-char p))
  (display (read-char p))
  (display (read-char p))
  (display (read-char p))
  (display (eof-object? (read-char p))))
