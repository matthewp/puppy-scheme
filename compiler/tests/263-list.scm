;; expect: 1231
(let ((l (list 1 2 3)))
  (display (car l))
  (display (car (cdr l)))
  (display (car (cdr (cdr l))))
  (display (null? (cdr (cdr (cdr l))))))
