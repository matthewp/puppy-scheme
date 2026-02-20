;; expect: 104101108
(let ((bv (make-bytevector 16)))
  (bytevector-copy-string! bv 0 "hel")
  (display (bytevector-u8-ref bv 0))
  (display (bytevector-u8-ref bv 1))
  (display (bytevector-u8-ref bv 2)))
