;; Test: named-let with cond (pattern from find-or-add-arity!)
;; expect: 42

(define v (make-vector 5 0))
(vector-set! v 4 0)

(define (find-val v target)
  (let ((count (vector-ref v 4)))
    (let loop ((j 0))
      (cond
        ((>= j count) (- 0 1))  ;; not found
        ((= (vector-ref v j) target) j)
        (else (loop (+ j 1)))))))

(vector-set! v 0 10)
(vector-set! v 1 20)
(vector-set! v 2 42)
(vector-set! v 4 3)

(display (vector-ref v (find-val v 42)))
(newline)
