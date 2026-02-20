;; expect: 2
;; expect: 3
;; expect: 4
(let ((result (map (lambda (x) (+ x 1)) '(1 2 3))))
  (display (car result))
  (newline)
  (display (car (cdr result)))
  (newline)
  (display (car (cdr (cdr result))))
  (newline))
