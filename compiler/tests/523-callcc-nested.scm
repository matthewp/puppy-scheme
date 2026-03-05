;; expect: 8
;; Nested call/cc: inner escape doesn't affect outer
;; inner call/cc: (k2 3) → returns 3; outer lambda body: (+ 1 3) = 4; (+ 4 4) = 8
(display (+ (call/cc (lambda (k1) (+ 1 (call/cc (lambda (k2) (k2 3)))))) 4))
