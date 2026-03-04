;;; display.scm — Display runtime function
;;; Generates the display function body conditionally based on
;;; which types the program uses and the target platform.

(define (make-rt-display fn-display
                          needs-flonum needs-rational needs-complex
                          needs-symbol)
  ;; I/O helper: output bytes from memory (uses fd param)
  (define (io ptr len)
    `(%stream-write fd ,ptr ,len 300))

  (define io-1 (io 48 1))

  ;; Integer display code (operates on val as i31)
  (define int-display
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

  ;; Char display
  (define char-display
    `(begin (%mem-store8 48 (%char-code val)) ,io-1 0))

  ;; String display: copy bytes to mem[48..] in chunks, then output each chunk
  ;; Max chunk size avoids overrunning linear memory (stay within one page)
  (define string-display
    (let ((chunk-size 4096))
      `(begin
         (set! cnt (%string-length val))
         (set! cursor 0)
         (%block-void
           (%loop-void
             (%br-if 1 (%i31-ge cursor cnt))
             ;; Compute chunk length: min(chunk-size, remaining)
             (set! frac (%i31-sub cnt cursor))
             (if (%i31-gt frac ,chunk-size)
                 (set! frac ,chunk-size) 0)
             ;; Copy chunk to mem[48..48+frac]
             (set! imag 0)
             (%block-void
               (%loop-void
                 (%br-if 1 (%i31-ge imag frac))
                 (%mem-store8 (%i31-add 48 imag)
                              (%string-ref val (%i31-add cursor imag)))
                 (set! imag (%i31-add imag 1))
                 (%br 0)))
             ,(io 48 'frac)
             (set! cursor (%i31-add cursor frac))
             (%br 0)))
         0)))

  ;; Flonum display
  (define flonum-display
    `(begin
       (set! frac val)
       ;; Handle negative
       (if (%f64-lt frac 0.0)
           (begin (%mem-store8 48 45) ,io-1 (set! frac (%f64-neg frac)))
           0)
       ;; Integer part: truncate and display via recursive call
       (set! val (%f64-trunc-to-i31 frac))
       (set! frac (%f64-sub frac (%f64-convert-i31 val)))
       (%call ,fn-display val fd)
       ;; Decimal point + fractional digits
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
       ;; If no fractional digits, write '0'
       (if (%i31-eqz cnt)
           (begin (%mem-store8 49 48) (set! cursor 50))
           0)
       ;; Output decimal part (from '.' at 48 to cursor)
       ,(io 48 '(%i31-sub cursor 48))
       0))

  ;; Rational display: num/den
  (define rational-display
    `(begin
       (%call ,fn-display (%rational-num val) fd)
       (%mem-store8 48 47)
       ,io-1
       (%call ,fn-display (%rational-den val) fd)
       0))

  ;; Complex display: real+imagi
  (define complex-display
    (let ((plus-output `(begin (%mem-store8 48 43) ,io-1 0)))
      `(begin
         (set! imag (%complex-imag val))
         (%call ,fn-display (%complex-real val) fd)
         ;; Check sign of imaginary to decide whether to output '+'
         ,(if needs-flonum
              `(if (%i31? imag)
                   (if (%i31-ge imag 0) ,plus-output 0)
                   (if (%f64-ge imag 0.0) ,plus-output 0))
              `(if (%i31-ge imag 0) ,plus-output 0))
         (%call ,fn-display imag fd)
         (%mem-store8 48 105)
         ,io-1
         0)))

  ;; Null display: output ()
  (define null-display
    `(begin
       (%mem-store8 48 40)   ;; (
       (%mem-store8 49 41)   ;; )
       ,(io 48 2)
       0))

  ;; Pair display: output (a b c) or (a . b)
  (define pair-display
    `(begin
       (%mem-store8 48 40)   ;; (
       ,io-1
       (%call ,fn-display (%car val) fd)
       (set! val (%cdr val))
       (%block-void
         (%loop-void
           (%br-if 1 (%ref-is-null val))
           (%br-if 1 (%i31-eqz (%pair? val)))
           (%mem-store8 48 32)  ;; space
           ,io-1
           (%call ,fn-display (%car val) fd)
           (set! val (%cdr val))
           (%br 0)))
       (if (%ref-is-null val) 0
           (begin
             (%mem-store8 48 32)  ;; space
             (%mem-store8 49 46)  ;; .
             (%mem-store8 50 32)  ;; space
             ,(io 48 3)
             (%call ,fn-display val fd)
             0))
       (%mem-store8 48 41)   ;; )
       ,io-1
       0))

  ;; Boolean display
  (define bool-display
    `(if (%ref-eq val #f)
         (begin (%mem-store8 48 35) (%mem-store8 49 102) ,(io 48 2) 0)
         (if (%ref-eq val #t)
             (begin (%mem-store8 48 35) (%mem-store8 49 116) ,(io 48 2) 0)
             ,int-display)))

  ;; Build type dispatch chain (inside out)
  (let* ((base `(if (%char? val) ,char-display ,string-display))
         (base (if needs-complex  `(if (%complex? val)  ,complex-display  ,base) base))
         (base (if needs-rational `(if (%rational? val) ,rational-display ,base) base))
         (base (if needs-flonum   `(if (%flonum? val)   ,flonum-display   ,base) base))
         (base `(if (%pair? val) ,pair-display ,base)))
    (list '("x" "fd") '("val" "cursor" "cnt" "frac" "imag")
      (list
        `(begin
           (set! val x)
           ,(if needs-symbol
                `(if (%symbol? val) (set! val (%symbol-string val)) 0)
                0)
           (if (%ref-is-null val)
               ,null-display
               (if (%i31? val)
                   ,bool-display
                   ,base))
           0)))))
