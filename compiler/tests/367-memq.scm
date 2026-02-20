;; expect: 2
;; expect: 3
;; expect: nope
(let ((r (memq 2 (list 1 2 3))))
  (if (pair? r)
      (begin (display (car r)) (newline)
             (display (car (cdr r))) (newline))
      (begin (display "nope") (newline))))
(let ((r (memq 9 (list 1 2 3))))
  (if (pair? r)
      (begin (display (car r)) (newline))
      (begin (display "nope") (newline))))
