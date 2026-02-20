;; expect: 012345
(let loop ((i 0))
  (if (< i 6)
      (begin
        (display i)
        (loop (+ i 1)))))
