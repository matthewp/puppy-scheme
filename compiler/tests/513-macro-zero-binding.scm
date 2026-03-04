;; expect: found
;; Test that syntax-rules pattern variable matching value 0 works
(define-syntax my-test
  (syntax-rules ()
    ((my-test (v i))
     (begin (display i) (newline)))))
(define-syntax my-test2
  (syntax-rules (state)
    ((my-test2 (state (x init)) body ...)
     (let ((x init)) body ...))))
(my-test2 (state (count 0))
  (display "found")
  (newline))
