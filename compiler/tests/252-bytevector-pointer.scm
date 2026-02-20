;; expect: 4096
(let ((bv (make-bytevector 8)))
  (display (bytevector->pointer bv)))
