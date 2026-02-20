;; Test: named-let with display calls inside, capturing outer let var
;; Reproduces exact find-or-add-arity! crash pattern with debug prints
;; expect: j=0 count=3
;; expect: j=1 count=3
;; expect: j=2 count=3
;; expect: j=3 count=3
;; expect: result=13

(define (find-val vec n start)
  (let ((count (vector-ref vec 0)))
    (let loop ((j 0))
      (display "j=")
      (display j)
      (display " count=")
      (display count)
      (newline)
      (if (>= j count)
          (+ start count)
          (loop (+ j 1))))))

(define v (make-vector 5 0))
(vector-set! v 0 3)
(let ((r (find-val v 99 10)))
  (display "result=")
  (display r)
  (newline))
