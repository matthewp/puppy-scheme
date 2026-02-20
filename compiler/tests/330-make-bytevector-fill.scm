;; expect: 65
(let ((bv (make-bytevector 4 65)))
  (display (bytevector-u8-ref bv 2)))
