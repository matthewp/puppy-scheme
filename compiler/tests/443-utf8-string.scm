;; expect: hello
;; expect: ell
;; expect: 5
;; expect: 104 101 108 108 111
;; expect: 3
;; expect: 108 108 111
(let ((bv (string->utf8 "hello")))
  ;; round-trip: bv -> string
  (display (utf8->string bv))
  (newline)
  ;; sub-range: utf8->string with start/end
  (display (utf8->string bv 1 4))
  (newline)
  ;; verify bytevector contents
  (display (bytevector-length bv))
  (newline)
  (display (bytevector-u8-ref bv 0))
  (display " ")
  (display (bytevector-u8-ref bv 1))
  (display " ")
  (display (bytevector-u8-ref bv 2))
  (display " ")
  (display (bytevector-u8-ref bv 3))
  (display " ")
  (display (bytevector-u8-ref bv 4))
  (newline)
  ;; string->utf8 with start/end
  (let ((bv2 (string->utf8 "hello" 2)))
    (display (bytevector-length bv2))
    (newline)
    (display (bytevector-u8-ref bv2 0))
    (display " ")
    (display (bytevector-u8-ref bv2 1))
    (display " ")
    (display (bytevector-u8-ref bv2 2))
    (newline)))
