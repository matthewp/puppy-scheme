;; expect: 42
(let ((a (make-bytevector 4 0))
      (b (make-bytevector 4 42)))
  (bytevector-copy! a 1 b 0 3)
  (display (bytevector-u8-ref a 1)))
