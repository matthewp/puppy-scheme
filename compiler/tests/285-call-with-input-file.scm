
;; wasi-dir: tests
;; expect: h
(display (call-with-input-file "lib/hello.txt"
           (lambda (p) (read-char p))))
(newline)
