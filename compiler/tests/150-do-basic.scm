;; expect: 012345
(do ((i 0 (+ i 1)))
    ((= i 6))
  (display i))
