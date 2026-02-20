;; expect: #t
(letrec ((even? (lambda (n)
                  (if (= n 0) #t (odd? (- n 1)))))
         (odd? (lambda (n)
                 (if (= n 0) #f (even? (- n 1))))))
  (if (even? 10)
      (display "#t")
      (display "#f")))
