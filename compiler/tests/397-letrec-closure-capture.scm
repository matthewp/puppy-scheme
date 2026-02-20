;; Test: letrec with closures capturing outer let-bound variable
;; Pattern from find-free-vars in analyze.scm
;; expect: 15

(define (test-pattern data)
  (let ((result 0))
    (letrec ((add! (lambda (v) (set! result (+ result v))))
             (walk (lambda (lst)
                     (when (pair? lst)
                       (add! (car lst))
                       (walk (cdr lst))))))
      (walk data)
      result)))

(display (test-pattern '(1 2 3 4 5)))
(newline)
