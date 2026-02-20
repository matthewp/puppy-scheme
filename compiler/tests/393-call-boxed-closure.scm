;; Test: calling a closure stored in a boxed variable
;; The loop variable from named-let is set! + self-captured = boxed
;; expect: 15

(define (sum-list lst)
  (let ((total 0))
    (let loop ((xs lst))
      (when (pair? xs)
        (set! total (+ total (car xs)))
        (loop (cdr xs))))
    total))

(display (sum-list '(1 2 3 4 5)))
(newline)
