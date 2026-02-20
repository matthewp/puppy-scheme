;; expect: 2010
(define x 10)
(letrec ((f (lambda () 20)))
  (display (f)))
(display x)
