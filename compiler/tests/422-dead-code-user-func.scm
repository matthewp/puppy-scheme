;; Test: unused user-defined functions are eliminated
;; expect: 42
(define (unused x) (+ x 1))
(display 42)
