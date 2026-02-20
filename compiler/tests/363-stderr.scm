;; expect: hello
;; expect-stderr: error
(display "hello")
(display "error" (current-error-port))
