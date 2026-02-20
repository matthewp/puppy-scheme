;; expect: 30
(define-external (add (i32 x) (i32 y)) i32
  (+ x y))
(define-external (mul (i32 x) (i32 y)) i32
  (* x y))
(display (add (mul 3 4) (mul 2 9)))
