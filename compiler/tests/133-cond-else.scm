;; expect: fallback
(cond
  ((= 1 2) (display "nope"))
  (else (display "fallback")))
