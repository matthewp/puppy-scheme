;; expect: 12345
(let ((bv (make-bytevector 16)))
  (bytevector-u32-native-set! bv 0 12345)
  (display (bytevector-u32-native-ref bv 0)))
