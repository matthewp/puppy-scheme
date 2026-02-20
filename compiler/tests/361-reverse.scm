;; expect: 3
;; expect: 2
;; expect: 1
(let ((r (reverse '(1 2 3))))
  (display (car r))
  (newline)
  (display (car (cdr r)))
  (newline)
  (display (car (cdr (cdr r))))
  (newline))
