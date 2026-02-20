;; expect: two
(case (+ 1 1)
  ((1) (display "one"))
  ((2) (display "two"))
  ((3) (display "three")))
