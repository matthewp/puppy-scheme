;; expect: found
;; call/cc used as early exit from a search loop
(define (search lst pred)
  (call/cc
    (lambda (return)
      (for-each (lambda (x)
                  (when (pred x) (return 'found)))
                lst)
      'not-found)))
(display (search '(1 2 3 4 5) (lambda (x) (= x 3))))
