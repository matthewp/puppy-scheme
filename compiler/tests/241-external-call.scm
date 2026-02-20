;; expect: 42
(define-external (add (i32 x) (i32 y)) i32
  (+ x y))
(display (add 20 22))
