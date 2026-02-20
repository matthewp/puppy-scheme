;; expect: 0
;; pair? on null (empty list) should be false
(display (if (pair? '()) 1 0))
(newline)
