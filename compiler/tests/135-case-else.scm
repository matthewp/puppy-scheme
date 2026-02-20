;; expect: other
(case 99
  ((1) (display "one"))
  ((2) (display "two"))
  (else (display "other")))
