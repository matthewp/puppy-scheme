;; expect: 100
(display (if (exact? 42) 1 0))
(display (if (inexact? 42) 1 0))
(display (if (inexact? "hi") 1 0))
