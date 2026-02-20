;; Test: named-let loop capturing variable from outer let
;; Minimal reproduction of find-or-add-arity! crash pattern
;; expect: 5

(define (count-up-to v)
  (let ((limit (vector-ref v 0)))
    (let loop ((i 0))
      (if (>= i limit)
          i
          (loop (+ i 1))))))

(define v (make-vector 1 5))
(display (count-up-to v))
(newline)
