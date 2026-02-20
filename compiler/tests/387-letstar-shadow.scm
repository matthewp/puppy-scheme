;; expect: 11
;; expect: 7
;; let* with same-name shadowing - second binding should see first
(let* ((x 10)
       (x (+ x 1)))
  (display x)
  (newline))
(let* ((y 5)
       (y (+ y 1))
       (y (+ y 1)))
  (display y)
  (newline))
