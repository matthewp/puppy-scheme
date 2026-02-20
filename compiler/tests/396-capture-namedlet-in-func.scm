;; Test: named-let inside function capturing function parameter
;; This makes loop a local variable (boxed: set! + self-captured)
;; and count a captured parameter
;; expect: 5

(define (count-to count)
  (let loop ((j 0))
    (if (>= j count)
        j
        (loop (+ j 1)))))

(display (count-to 5))
(newline)
