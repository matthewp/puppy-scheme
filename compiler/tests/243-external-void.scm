;; expect: hello
(define-external (greet) void
  (display "hello"))
(greet)
