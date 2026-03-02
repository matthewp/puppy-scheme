;; expect: xxxxx
(let ((s (make-string 5 #\a)))
  (string-fill! s #\x)
  (display s))
