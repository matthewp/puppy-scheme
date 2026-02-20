;; expect: 3
;; 6
;; 25
;; 30
(display (apply (lambda (x y) (+ x y)) (list 1 2)))
(newline)
(display (apply (lambda (a b c) (+ a (+ b c))) 1 2 (list 3)))
(newline)
(display (apply (lambda (x) (* x x)) (list 5)))
(newline)
(define (add a b) (+ a b))
(display (apply add (list 10 20)))
(newline)
