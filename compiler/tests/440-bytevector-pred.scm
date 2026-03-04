;; expect: #t
;; expect: #f
;; expect: #f
(display (bytevector? (make-bytevector 3)))
(newline)
(display (bytevector? "hi"))
(newline)
(display (bytevector? 42))
(newline)
