;; expect: 0
;; expect: 2
;; expect: 10 20
;; expect: 4
;; expect: 10 20 30 40
;; expect: 6
(let ((a (make-bytevector 2 0))
      (b (make-bytevector 2 0))
      (c (make-bytevector 2 0)))
  (bytevector-u8-set! a 0 10)
  (bytevector-u8-set! a 1 20)
  (bytevector-u8-set! b 0 30)
  (bytevector-u8-set! b 1 40)
  (bytevector-u8-set! c 0 50)
  (bytevector-u8-set! c 1 60)
  ;; 0-arg
  (display (bytevector-length (bytevector-append)))
  (newline)
  ;; 1-arg
  (let ((r (bytevector-append a)))
    (display (bytevector-length r))
    (newline)
    (display (bytevector-u8-ref r 0))
    (display " ")
    (display (bytevector-u8-ref r 1))
    (newline))
  ;; 2-arg
  (let ((r (bytevector-append a b)))
    (display (bytevector-length r))
    (newline)
    (display (bytevector-u8-ref r 0))
    (display " ")
    (display (bytevector-u8-ref r 1))
    (display " ")
    (display (bytevector-u8-ref r 2))
    (display " ")
    (display (bytevector-u8-ref r 3))
    (newline))
  ;; 3-arg
  (display (bytevector-length (bytevector-append a b c)))
  (newline))
