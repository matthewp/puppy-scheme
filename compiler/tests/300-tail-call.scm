;; expect: done
(define (loop n)
  (if (= n 0)
      (display "done")
      (loop (- n 1))))
(loop 1000000)
(newline)
