;; expect: hel
;; expect: abc
(let ((lst (string->list "hel")))
  (display (car lst))
  (display (car (cdr lst)))
  (display (car (cdr (cdr lst)))))
(newline)
(display (list->string (list #\a #\b #\c)))
(newline)
