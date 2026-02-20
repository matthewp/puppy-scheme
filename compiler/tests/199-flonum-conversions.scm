;; expect: 3.0 3 2.0 -3.0
(display (exact->inexact 3))
(display " ")
(display (inexact->exact 3.7))
(display " ")
(display (floor 2.7))
(display " ")
(display (ceiling -3.2))
