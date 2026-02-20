;; Test: boxing analysis name collision
;; A variable named "x" is set! + captured in one scope, while
;; a different variable also named "x" is captured (but NOT set!) in another.
;; The boxing analysis must not confuse the two bindings.
;; expect: 10

;; This function has "x" that IS set! and captured → needs boxing
(define (mutate-counter)
  (let ((x 0))
    (let loop ((i 0))
      (if (>= i 5)
          x
          (begin
            (set! x (+ x 1))
            (loop (+ i 1)))))))

;; This function has "x" that is NOT set! but IS captured → no boxing needed
(define (read-counter v)
  (let ((x (vector-ref v 0)))
    (let loop ((j 0))
      (if (>= j x)
          j
          (loop (+ j 1))))))

(define v (make-vector 1 10))
(display (read-counter v))
(newline)
