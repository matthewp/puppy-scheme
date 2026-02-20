
;; wasi-dir: tests
;; expect: yes no 97 32
(call-with-input-file "data/bools-chars.txt"
  (lambda (port)
    ;; Read #t — truthy
    (let ((x (read port)))
      (if x (display "yes") (display "no")))
    (display " ")
    ;; Read #f — falsy
    (let ((x (read port)))
      (if x (display "yes") (display "no")))
    (display " ")
    ;; Read #\a — char with code 97
    (display (char->integer (read port)))
    (display " ")
    ;; Read #\space — char with code 32
    (display (char->integer (read port)))
    (newline)))
