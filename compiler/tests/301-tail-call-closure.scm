;; expect: done
(letrec ((f (lambda (n)
              (if (= n 0)
                  (display "done")
                  (f (- n 1))))))
  (f 1000000))
(newline)
