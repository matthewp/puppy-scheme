
;; wasi-dir: tests
;; expect: hhello
(let ((p (open-input-file "lib/hello.txt")))
  (let ((c (peek-char p)))
    (display c))
  (let loop ()
    (let ((c (read-char p)))
      (if (eof-object? c)
          (close-input-port p)
          (begin (display c) (loop))))))
(newline)
