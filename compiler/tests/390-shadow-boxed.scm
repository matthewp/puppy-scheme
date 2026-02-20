;; Test: inner let binding shadows an outer boxed variable
;; The inner binding should NOT be treated as boxed
;; expect: 10

(let ((x 0))
  ;; x is set! and captured → boxed
  (for-each (lambda (n) (set! x (+ x n))) '(1 2 3 4))
  ;; Inner let shadows x with a non-boxed binding
  (let ((x (+ x 0)))
    (display x)
    (newline)))
