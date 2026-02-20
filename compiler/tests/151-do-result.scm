;; expect: 55
(display
  (do ((i 0 (+ i 1))
       (sum 0 (+ sum i)))
      ((= i 11) sum)))
