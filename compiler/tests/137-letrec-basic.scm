;; expect: 120
(letrec ((fact (lambda (n)
                 (if (= n 0) 1
                     (* n (fact (- n 1)))))))
  (display (fact 5)))
