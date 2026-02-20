;; expect: 200
(let ((alist (list (cons "a" 10) (cons "b" 20) (cons "c" 30))))
  (let ((result (assoc "b" alist)))
    (display (cdr result)))
  (display (if (assoc "z" alist) 1 0)))
