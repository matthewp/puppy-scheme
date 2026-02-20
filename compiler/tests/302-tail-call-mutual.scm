;; expect: done
(define (even? n)
  (if (= n 0) #t (odd? (- n 1))))
(define (odd? n)
  (if (= n 0) #f (even? (- n 1))))
(display (if (even? 1000000) "done" "fail"))
(newline)
