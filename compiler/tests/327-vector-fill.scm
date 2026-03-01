;; expect: 42
(let ((v (make-vector 3 0)))
  (vector-fill! v 42)
  (display (vector-ref v 1)))
