
;; wasi-dir: tests
;; expect: 1 2 3 a.b 4
(call-with-input-file "data/lists.txt"
  (lambda (port)
    ;; Read (1 2 3)
    (let ((lst (read port)))
      (display (car lst))
      (display " ")
      (display (car (cdr lst)))
      (display " ")
      (display (car (cdr (cdr lst)))))
    (display " ")
    ;; Read (a . b)
    (let ((p (read port)))
      (display (car p))
      (display ".")
      (display (cdr p)))
    (display " ")
    ;; Read ((4 5) (6 7))
    (let ((nested (read port)))
      (display (car (car nested))))
    (newline)))
