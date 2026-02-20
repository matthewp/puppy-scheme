
;; wasi-dir: tests
;; expect: hello
(let ((p (open-input-file "lib/hello.txt")))
  (let loop ()
    (let ((c (read-char p)))
      (if (eof-object? c)
          (close-input-port p)
          (begin (display c) (loop))))))
(newline)
