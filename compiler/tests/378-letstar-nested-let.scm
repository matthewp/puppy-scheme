;; expect: 1142
;; let* where init expression contains a nested let
(let* ((a (let ((tmp 10)) (+ tmp 1)))
       (b 42))
  (display a)
  (display b))
