;; expect: 12345
;; Test that if/and/or/cond/not handle non-i31 values correctly
;; In Scheme, everything except #f is truthy

;; String in if test
(if "hello" (display 1) (display 0))
;; Pair in if test
(if (cons 1 2) (display 2) (display 0))
;; and with string
(if (and #t "ok") (display 3) (display 0))
;; not with string
(if (not "hello") (display 0) (display 4))
;; cond with pair test
(cond ((cons 'a 'b) (display 5))
      (else (display 0)))
(newline)
