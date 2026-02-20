
;; wasi-dir: tests
;; expect: 5 hello
(call-with-input-file "data/strings.txt"
  (lambda (port)
    (let ((s (read port)))
      (display (string-length s))
      (display " ")
      (display s))
    (newline)))
