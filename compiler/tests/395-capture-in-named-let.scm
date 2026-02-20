;; Test: accessing outer let-bound variable from inside a named-let
;; Reproduces find-or-add-arity! pattern
;; expect: 5

(display
  (let ((count 5))
    (let loop ((j 0))
      (if (>= j count)
          j
          (loop (+ j 1))))))
(newline)
