
;; wasi-dir: tests
;; expect: #t
;; expect: #f
(let ((p (open-input-file "lib/empty.txt")))
  (let ((c (read-char p)))
    (display (eof-object? c))
    (newline))
  (close-input-port p))
(display (eof-object? 42))
(newline)
