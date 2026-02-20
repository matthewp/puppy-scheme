
;; wasi-dir: tests
;; expect: 110010
(let ((p (open-input-file "lib/hello.txt")))
  (display (port? p))
  (display (input-port? p))
  (display (output-port? p))
  (close-input-port p))
(display (port? 42))
(let ((p (open-output-file "lib/tmp-pred-test.txt")))
  (display (output-port? p))
  (display (input-port? p))
  (close-output-port p))
(newline)
