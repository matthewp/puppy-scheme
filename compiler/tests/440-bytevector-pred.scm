;; expect: 1
;; expect: 0
;; expect: 0
(display (bytevector? (make-bytevector 3)))
(newline)
(display (bytevector? "hi"))
(newline)
(display (bytevector? 42))
(newline)
