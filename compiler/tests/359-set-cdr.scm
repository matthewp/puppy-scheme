;; expect: 42
(let ((p (cons 1 2)))
  (set-cdr! p 42)
  (display (cdr p)))
