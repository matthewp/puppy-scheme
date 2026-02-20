;; expect: 42.0 -3.0 42 -3
(display (exact->inexact 42))
(display " ")
(display (exact->inexact -3))
(display " ")
(display (inexact->exact 42))
(display " ")
(display (inexact->exact -3))
