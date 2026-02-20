;; expect: 40964100
(let ((a (linear-alloc 4 4))
      (b (linear-alloc 100 4)))
  (display a)
  (display b))
