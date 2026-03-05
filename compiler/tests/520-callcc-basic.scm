;; expect: 42
;; Basic call/cc: escape from a computation
(display (call-with-current-continuation
          (lambda (k) (k 42) 99)))
