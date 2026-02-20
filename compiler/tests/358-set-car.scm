;; expect: 99
(let ((p (cons 1 2)))
  (set-car! p 99)
  (display (car p)))
