;; expect: 3
;; expect: 10 20 30
;; expect: 2
;; expect: 20 30
;; expect: #t
(let ((bv (make-bytevector 3 0)))
  (bytevector-u8-set! bv 0 10)
  (bytevector-u8-set! bv 1 20)
  (bytevector-u8-set! bv 2 30)
  ;; 1-arg copy
  (let ((c (bytevector-copy bv)))
    (display (bytevector-length c))
    (newline)
    (display (bytevector-u8-ref c 0))
    (display " ")
    (display (bytevector-u8-ref c 1))
    (display " ")
    (display (bytevector-u8-ref c 2))
    (newline))
  ;; 3-arg copy (sub-range)
  (let ((c2 (bytevector-copy bv 1 3)))
    (display (bytevector-length c2))
    (newline)
    (display (bytevector-u8-ref c2 0))
    (display " ")
    (display (bytevector-u8-ref c2 1))
    (newline))
  ;; verify independence
  (let ((c3 (bytevector-copy bv)))
    (bytevector-u8-set! c3 0 99)
    (display (= (bytevector-u8-ref bv 0) 10))
    (newline)))
