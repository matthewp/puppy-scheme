
;; wasi-dir: tests
;; expect: define fact n
(call-with-input-file "data/mixed.txt"
  (lambda (port)
    ;; Read (define (fact n) (if (= n 1) 1 (* n (fact (- n 1)))))
    (let ((form (read port)))
      ;; form is (define (fact n) ...)
      (display (car form))
      (display " ")
      ;; (car (cadr form)) is fact
      (display (car (car (cdr form))))
      (display " ")
      ;; (cadr (cadr form)) is n
      (display (car (cdr (car (cdr form)))))
      (newline))))
