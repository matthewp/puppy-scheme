;; expect: Aello
(let ((s (make-string 5 #\space)))
  (string-set! s 0 #\A)
  (string-set! s 1 #\e)
  (string-set! s 2 #\l)
  (string-set! s 3 #\l)
  (string-set! s 4 #\o)
  (display s))
(newline)
