;; expect: 23
;; expect: nope
(let ((r (member (list 2 3) (list (list 1 2) (list 2 3) (list 4 5)))))
  (if (pair? r)
      (begin (display (car (car r)))
             (display (car (cdr (car r))))
             (newline))
      (begin (display "nope") (newline))))
(let ((r (member (list 9 9) (list (list 1 2) (list 2 3)))))
  (if (pair? r)
      (begin (display "found") (newline))
      (begin (display "nope") (newline))))
