;; expect: 4265
(let ((bv (make-bytevector 10)))
  (bytevector-u8-set! bv 0 42)
  (bytevector-u8-set! bv 1 65)
  (display (bytevector-u8-ref bv 0))
  (display (bytevector-u8-ref bv 1)))
