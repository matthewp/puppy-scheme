;;; write.scm — Write runtime function
;;; Generates the write function body conditionally based on
;;; which types the program uses and the target platform.
;;; Unlike display, write outputs values in Scheme-readable form:
;;; - strings are quoted with "
;;; - characters are output as #\x
;;; - pairs/lists are output as (a b c) or (a . b)
;;; - null is output as ()

(define (make-rt-write fn-write
                       needs-flonum needs-rational needs-complex
                       needs-symbol)
  ;; I/O helper: output bytes from memory (uses fd param)
  (define (io ptr len)
    `(%stream-write fd ,ptr ,len 300))

  (define io-1 (io 48 1))

  ;; Integer write: same as display
  (define int-write
    `(if (%i31-eqz val)
         (begin (%mem-store8 48 48) ,io-1 0)
         (begin
           (if (%i31-lt val 0)
               (begin (%mem-store8 48 45) ,io-1 (set! val (%i31-neg val)))
               0)
           (set! cursor 95)
           (%block-void
             (%loop-void
               (%br-if 1 (%i31-eqz val))
               (%mem-store8 cursor (%i31-add (%i31-rem-u val 10) 48))
               (set! cursor (%i31-sub cursor 1))
               (set! val (%i31-div-u val 10))
               (%br 0)))
           ,(io '(%i31-add cursor 1) '(%i31-sub 95 cursor))
           0)))

  ;; Char write: output #\x
  (define char-write
    `(begin
       (%mem-store8 48 35)   ;; #
       (%mem-store8 49 92)   ;; backslash
       (%mem-store8 50 (%char-code val))
       ,(io 48 3)
       0))

  ;; String write: output "..." with quotes
  (define string-write
    `(begin
       (%mem-store8 48 34)   ;; opening "
       ,io-1
       (set! cnt (%string-length val))
       (set! cursor 0)
       (%block-void
         (%loop-void
           (%br-if 1 (%i31-ge cursor cnt))
           (%mem-store8 48 (%string-ref val cursor))
           ,io-1
           (set! cursor (%i31-add cursor 1))
           (%br 0)))
       (%mem-store8 48 34)   ;; closing "
       ,io-1
       0))

  ;; Symbol write: just display the name (no quotes)
  (define symbol-write
    `(begin
       (set! val (%symbol-string val))
       (set! cnt (%string-length val))
       (set! cursor 0)
       (%block-void
         (%loop-void
           (%br-if 1 (%i31-ge cursor cnt))
           (%mem-store8 (%i31-add 48 cursor) (%string-ref val cursor))
           (set! cursor (%i31-add cursor 1))
           (%br 0)))
       ,(io 48 'cnt)
       0))

  ;; Null write: output ()
  (define null-write
    `(begin
       (%mem-store8 48 40)   ;; (
       (%mem-store8 49 41)   ;; )
       ,(io 48 2)
       0))

  ;; Pair write: output (a b c) or (a . b)
  ;; Strategy: print car, then loop on cdr while it's a pair.
  ;; After loop, check if remainder is null (proper) or not (dotted).
  (define pair-write
    `(begin
       (%mem-store8 48 40)   ;; (
       ,io-1
       (%call ,fn-write (%car val) fd)
       (set! val (%cdr val))
       (%block-void
         (%loop-void
           (%br-if 1 (%ref-is-null val))
           (%br-if 1 (%i31-eqz (%pair? val)))
           (%mem-store8 48 32)  ;; space
           ,io-1
           (%call ,fn-write (%car val) fd)
           (set! val (%cdr val))
           (%br 0)))
       (if (%ref-is-null val) 0
           (begin
             (%mem-store8 48 32)  ;; space
             (%mem-store8 49 46)  ;; .
             (%mem-store8 50 32)  ;; space
             ,(io 48 3)
             (%call ,fn-write val fd)
             0))
       (%mem-store8 48 41)   ;; )
       ,io-1
       0))

  ;; Flonum write: same as display
  (define flonum-write
    `(begin
       (set! frac val)
       (if (%f64-lt frac 0.0)
           (begin (%mem-store8 48 45) ,io-1 (set! frac (%f64-neg frac)))
           0)
       (set! val (%f64-trunc-to-i31 frac))
       (set! frac (%f64-sub frac (%f64-convert-i31 val)))
       (%call ,fn-write val fd)
       (%mem-store8 48 46)
       (set! cursor 49)
       (set! cnt 0)
       (%block-void
         (%loop-void
           (%br-if 1 (%i31-ge cnt 6))
           (%br-if 1 (%f64-lt frac 0.0000001))
           (set! frac (%f64-mul frac 10.0))
           (set! val (%f64-trunc-to-i31 frac))
           (%mem-store8 cursor (%i31-add val 48))
           (set! cursor (%i31-add cursor 1))
           (set! cnt (%i31-add cnt 1))
           (set! frac (%f64-sub frac (%f64-convert-i31 val)))
           (%br 0)))
       (if (%i31-eqz cnt)
           (begin (%mem-store8 49 48) (set! cursor 50))
           0)
       ,(io 48 '(%i31-sub cursor 48))
       0))

  ;; Rational write: num/den
  (define rational-write
    `(begin
       (%call ,fn-write (%rational-num val) fd)
       (%mem-store8 48 47)
       ,io-1
       (%call ,fn-write (%rational-den val) fd)
       0))

  ;; Complex write: real+imagi
  (define complex-write
    (let ((plus-output `(begin (%mem-store8 48 43) ,io-1 0)))
      `(begin
         (set! imag (%complex-imag val))
         (%call ,fn-write (%complex-real val) fd)
         ,(if needs-flonum
              `(if (%i31? imag)
                   (if (%i31-ge imag 0) ,plus-output 0)
                   (if (%f64-ge imag 0.0) ,plus-output 0))
              `(if (%i31-ge imag 0) ,plus-output 0))
         (%call ,fn-write imag fd)
         (%mem-store8 48 105)
         ,io-1
         0)))

  ;; Build type dispatch chain
  ;; write differs from display: it handles pairs and null,
  ;; quotes strings, and outputs #\x for chars
  (let* ((base `(if (%char? val) ,char-write
                    (if (%symbol? val) ,symbol-write
                        ,string-write)))
         (base (if needs-complex  `(if (%complex? val)  ,complex-write  ,base) base))
         (base (if needs-rational `(if (%rational? val) ,rational-write ,base) base))
         (base (if needs-flonum   `(if (%flonum? val)   ,flonum-write   ,base) base))
         (base `(if (%pair? val) ,pair-write ,base)))
    (list '("x" "fd") '("val" "cursor" "cnt" "frac" "imag")
      (list
        `(begin
           (set! val x)
           (if (%ref-is-null val)
               ,null-write
               (if (%i31? val)
                   ,int-write
                   ,base))
           0)))))
