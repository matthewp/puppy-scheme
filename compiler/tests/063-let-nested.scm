;; expect: 6
(display (let ((x 1))
           (let ((y 2))
             (let ((z 3))
               (+ x (+ y z))))))
