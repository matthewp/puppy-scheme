;; expect: nofalse
(unless #f (display "no"))
(unless #t (display "yes"))
(display "false")
