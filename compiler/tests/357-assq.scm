;; expect: 200
(let ((alist (list (cons 1 10) (cons 2 20) (cons 3 30))))
  (let ((result (assq 2 alist)))
    (display (cdr result)))
  (display (if (assq 9 alist) 1 0)))
