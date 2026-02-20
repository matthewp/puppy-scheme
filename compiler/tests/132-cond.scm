;; expect: two
(cond
  ((= 1 2) (display "one"))
  ((= 2 2) (display "two"))
  (else (display "other")))
