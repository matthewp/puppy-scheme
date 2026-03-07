;; expect: 10
;; call/cc where continuation is not invoked — returns normally
(display (call/cc
          (lambda (k) (+ 3 7))))
