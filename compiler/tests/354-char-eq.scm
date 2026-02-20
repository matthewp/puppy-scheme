;; expect: 101
(display (if (char=? #\a #\a) 1 0))
(display (if (char=? #\a #\b) 1 0))
(display (if (char=? #\space #\space) 1 0))
