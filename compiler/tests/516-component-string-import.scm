;; wit: tests/wit/lookup.wit
;; validate-component: true
;; expect: component valid
(define (greet id)
  (string-append "Hello, " (get-name id) "!"))
