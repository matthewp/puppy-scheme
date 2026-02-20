;; expect: 1234
(let ((r (append '(1 2) '(3 4))))
  (display (car r))
  (display (car (cdr r)))
  (display (car (cdr (cdr r))))
  (display (car (cdr (cdr (cdr r))))))
