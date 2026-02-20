;; expect: 42
;; expect: hello
;; Macro templates that reference user-defined functions (free references)
;; should not gensym those names — only introduced bindings get renamed.
(define (identity x) x)
(define (greet) (display "hello") (newline))

(define-syntax wrap
  (syntax-rules ()
    ((wrap val)
     (identity val))))

(define-syntax invoke
  (syntax-rules ()
    ((invoke)
     (greet))))

(display (wrap 42))
(newline)
(invoke)
