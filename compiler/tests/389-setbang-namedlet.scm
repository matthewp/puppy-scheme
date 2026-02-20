;; expect: 5
;; set! on a captured variable inside a named-let loop
(let ((count 0))
  (let loop ((i 0))
    (when (< i 5)
      (set! count (+ count 1))
      (loop (+ i 1))))
  (display count)
  (newline))
