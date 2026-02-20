;; expect: Hi
(let ((bv (make-bytevector 5)))
  (bytevector-u8-set! bv 0 72)
  (bytevector-u8-set! bv 1 105)
  (display (pointer->string (bytevector->pointer bv) 2)))
