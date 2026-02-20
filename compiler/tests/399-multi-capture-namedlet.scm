;; Test: named-let capturing multiple outer variables
;; Matches find-or-add-arity! pattern exactly: 3 func params + 1 let-bound
;; expect: 13

(define (find-val vec n start)
  (let ((count (vector-ref vec 0)))
    (let loop ((j 0))
      (if (>= j count)
          (+ start count)
          (if (= (vector-ref vec (+ j 1)) n)
              (+ start j)
              (loop (+ j 1)))))))

(define v (make-vector 5 0))
(vector-set! v 0 3)
(vector-set! v 1 10)
(vector-set! v 2 20)
(vector-set! v 3 30)
(display (find-val v 99 10))
(newline)
