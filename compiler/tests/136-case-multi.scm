;; expect: small
(case 3
  ((1 2 3) (display "small"))
  ((4 5 6) (display "big"))
  (else (display "other")))
