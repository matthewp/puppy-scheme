;; expect: 23
(define xs '(2 3))
(let ((result `(1 ,@xs 4)))
  (display (car (cdr result)))
  (display (car (cdr (cdr result)))))
