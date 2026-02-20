
;; wasi-dir: tests
;; expect: ABC
(let ((out (open-output-file "lib/tmp-write-test.txt")))
  (write-char #\A out)
  (write-char #\B out)
  (write-char #\C out)
  (close-output-port out))
(let ((inp (open-input-file "lib/tmp-write-test.txt")))
  (let loop ()
    (let ((c (read-char inp)))
      (if (eof-object? c)
          (close-input-port inp)
          (begin (display c) (loop))))))
(newline)
