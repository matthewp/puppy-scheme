;;; read.scm — S-expression reader runtime functions

;; make-rt-read: (port) → datum
;; Reads one S-expression from port. Returns eof object at end of input.
(define (make-rt-read fn-read fn-read-list fn-read-string fn-peek-char fn-read-char)
  ;; helper: test if code is whitespace
  (define (ws? c)
    `(if (%i31-eq ,c 32) #t
         (if (%i31-eq ,c 10) #t
             (if (%i31-eq ,c 9) #t
                 (%i31-eq ,c 13)))))

  ;; helper: test if code is a delimiter
  (define (delim? c)
    `(if (%i31-eq ,c 32) #t
         (if (%i31-eq ,c 10) #t
             (if (%i31-eq ,c 9) #t
                 (if (%i31-eq ,c 13) #t
                     (if (%i31-eq ,c 40) #t
                         (if (%i31-eq ,c 41) #t
                             (if (%i31-eq ,c 34) #t
                                 (%i31-eq ,c 59)))))))))

  ;; helper: wrap datum with a symbol name — (cons (string->symbol name) (cons datum nil))
  (define (wrap-symbol name)
    `(cons (string->symbol ,name)
           (cons (%call ,fn-read port) (%ref-null))))

  ;; Copy token buffer (mem[1024..1024+tok-len]) to a GC string, return symbol
  (define tok-to-symbol
    `(begin
       (set! ch (%make-string tok-len 0))
       (set! i 0)
       (%block-void
         (%loop-void
           (%br-if 1 (%i31-ge i tok-len))
           (%string-set! ch i (%mem-load8 (%i31-add 1024 i)))
           (set! i (%i31-add i 1))
           (%br 0)))
       (string->symbol ch)))

  ;; Try to parse token as rational (num/den); fall back to float or symbol
  ;; Precondition: i points at '/' in token, num has numerator digits, neg has sign
  (define try-rational
    `(begin
       ;; Skip the '/'
       (set! i (%i31-add i 1))
       (set! fdigits 0)  ;; reuse as denominator accumulator
       ;; Parse denominator digits
       (%block-void
         (%loop-void
           (%br-if 1 (%i31-ge i tok-len))
           (set! ch (%mem-load8 (%i31-add 1024 i)))
           (%br-if 1 (not (if (%i31-ge ch 48) (%i31-le ch 57) #f)))
           (set! fdigits (%i31-add (%i31-mul fdigits 10) (%i31-sub ch 48)))
           (set! i (%i31-add i 1))
           (%br 0)))
       ;; Valid rational if: consumed all chars AND denominator > 0
       (if (if (%i31-ge i tok-len) (%i31-gt fdigits 0) #f)
           (%make-rational (if (%i31-ne neg 0) (%i31-neg num) num) fdigits)
           ,tok-to-symbol)))

  ;; Try to parse token as complex number (e.g., 3+4i, -1-2i, +4i, -1i); fall back to symbol
  ;; Token is in mem[1024..1024+tok-len]
  (define try-complex-or-symbol
    `(if (if (%i31-gt tok-len 1) (%i31-eq (%mem-load8 (%i31-add 1024 (%i31-sub tok-len 1))) 105) #f)
         ;; Last char is 'i' — try complex
         (begin
           ;; Find the last '+' or '-' (scan backwards from before 'i')
           (set! xval tok-len)  ;; sentinel: not found
           (set! i (%i31-sub tok-len 2))
           (%block-void
             (%loop-void
               (%br-if 1 (%i31-lt i 0))
               (set! ch (%mem-load8 (%i31-add 1024 i)))
               (if (if (%i31-eq ch 43) #t (%i31-eq ch 45))
                   (begin (set! xval i) (%br 2))
                   0)
               (set! i (%i31-sub i 1))
               (%br 0)))
           ;; If no separator found, not a valid complex
           (if (%i31-eq xval tok-len)
               ,tok-to-symbol
               (begin
                 ;; Parse real part [0..xval)
                 (set! num 0)
                 (set! neg 0)
                 (set! i 0)
                 (if (%i31-gt xval 0)
                     (begin
                       ;; Check for sign on real part
                       (if (%i31-eq (%mem-load8 1024) 45)
                           (begin (set! neg 1) (set! i 1))
                           (if (%i31-eq (%mem-load8 1024) 43)
                               (set! i 1)
                               0))
                       ;; Parse real digits
                       (%block-void
                         (%loop-void
                           (%br-if 1 (%i31-ge i xval))
                           (set! ch (%mem-load8 (%i31-add 1024 i)))
                           (%br-if 1 (not (if (%i31-ge ch 48) (%i31-le ch 57) #f)))
                           (set! num (%i31-add (%i31-mul num 10) (%i31-sub ch 48)))
                           (set! i (%i31-add i 1))
                           (%br 0)))
                       ;; Verify consumed all real chars
                       (if (%i31-ge i xval) 0 (set! xval tok-len)))
                     0)
                 (if (%i31-eq xval tok-len)
                     ,tok-to-symbol
                     (begin
                       ;; Apply sign to real
                       (set! num (if (%i31-ne neg 0) (%i31-neg num) num))
                       ;; Parse imaginary part [xval+1..tok-len-1)
                       ;; The char at xval is the sign (+/-)
                       (set! xneg (%i31-eq (%mem-load8 (%i31-add 1024 xval)) 45))
                       (set! fdigits 0)
                       (set! i (%i31-add xval 1))
                       (set! ndigits 0)
                       (%block-void
                         (%loop-void
                           (%br-if 1 (%i31-ge i (%i31-sub tok-len 1)))
                           (set! ch (%mem-load8 (%i31-add 1024 i)))
                           (%br-if 1 (not (if (%i31-ge ch 48) (%i31-le ch 57) #f)))
                           (set! fdigits (%i31-add (%i31-mul fdigits 10) (%i31-sub ch 48)))
                           (set! ndigits (%i31-add ndigits 1))
                           (set! i (%i31-add i 1))
                           (%br 0)))
                       ;; Valid complex if consumed up to 'i'
                       (if (%i31-ge i (%i31-sub tok-len 1))
                           (begin
                             ;; If no imag digits (just +i or -i), imag = 1
                             (if (%i31-eq ndigits 0) (set! fdigits 1) 0)
                             (set! fdigits (if xneg (%i31-neg fdigits) fdigits))
                             (%make-complex-raw num fdigits))
                           ,tok-to-symbol))))))
         ,tok-to-symbol))

  ;; Try to parse token as floating-point number; fall back to symbol
  ;; Token is in mem[1024..1024+tok-len], sign in neg
  (define try-float-or-symbol
    `(begin
       ;; Compute start position (skip sign char if present)
       (set! i (if (if (%i31-gt tok-len 1)
                       (if (%i31-eq (%mem-load8 1024) 45) #t (%i31-eq (%mem-load8 1024) 43))
                       #f)
                   1 0))
       ;; Initialize f64 accumulator to 0.0
       (set! num (%f64-convert-i31 0))
       (set! code 0)     ;; has_dot flag
       (set! fdigits 0)  ;; fractional digit count
       (set! ndigits 0)  ;; total digit count
       ;; Parse mantissa: digits and at most one dot
       (%block-void
         (%loop-void
           (%br-if 1 (%i31-ge i tok-len))
           (set! ch (%mem-load8 (%i31-add 1024 i)))
           ;; Break if not digit and not dot
           (%br-if 1 (not (if (if (%i31-ge ch 48) (%i31-le ch 57) #f) #t (%i31-eq ch 46))))
           ;; If dot, set flag; if digit, accumulate
           (if (%i31-eq ch 46)
               (set! code 1)
               (begin
                 (set! num (%f64-add (%f64-mul num (%f64-convert-i31 10))
                                     (%f64-convert-i31 (%i31-sub ch 48))))
                 (set! ndigits (%i31-add ndigits 1))
                 (if (%i31-ne code 0) (set! fdigits (%i31-add fdigits 1)) 0)))
           (set! i (%i31-add i 1))
           (%br 0)))
       ;; Parse exponent if 'e' or 'E' follows
       (set! xval 0)
       (set! xneg 0)
       (if (if (%i31-lt i tok-len)
               (if (%i31-eq (%mem-load8 (%i31-add 1024 i)) 101) #t
                   (%i31-eq (%mem-load8 (%i31-add 1024 i)) 69))
               #f)
           (begin
             (set! i (%i31-add i 1))
             ;; Exponent sign
             (if (%i31-lt i tok-len)
                 (if (%i31-eq (%mem-load8 (%i31-add 1024 i)) 45)
                     (begin (set! xneg 1) (set! i (%i31-add i 1)))
                     (if (%i31-eq (%mem-load8 (%i31-add 1024 i)) 43)
                         (set! i (%i31-add i 1))
                         0))
                 0)
             ;; Exponent digits
             (%block-void
               (%loop-void
                 (%br-if 1 (%i31-ge i tok-len))
                 (set! ch (%mem-load8 (%i31-add 1024 i)))
                 (%br-if 1 (not (if (%i31-ge ch 48) (%i31-le ch 57) #f)))
                 (set! xval (%i31-add (%i31-mul xval 10) (%i31-sub ch 48)))
                 (set! i (%i31-add i 1))
                 (%br 0))))
           0)
       ;; Valid float if: consumed all chars AND has digits AND (has dot OR has exponent)
       (if (if (%i31-ge i tok-len) (if (%i31-gt ndigits 0) (if (%i31-ne code 0) #t (if (%i31-ne xval 0) #t (%i31-ne xneg 0))) #f) #f)
           (begin
             ;; Apply fractional scaling: compute 10^fdigits, then divide once
             (set! ndigits (%f64-convert-i31 1))
             (%block-void
               (%loop-void
                 (%br-if 1 (%i31-le fdigits 0))
                 (set! ndigits (%f64-mul ndigits (%f64-convert-i31 10)))
                 (set! fdigits (%i31-sub fdigits 1))
                 (%br 0)))
             (set! num (%f64-div num ndigits))
             ;; Apply exponent: compute 10^xval, then multiply or divide once
             (set! ndigits (%f64-convert-i31 1))
             (%block-void
               (%loop-void
                 (%br-if 1 (%i31-le xval 0))
                 (set! ndigits (%f64-mul ndigits (%f64-convert-i31 10)))
                 (set! xval (%i31-sub xval 1))
                 (%br 0)))
             (if (%i31-ne xneg 0)
                 (set! num (%f64-div num ndigits))
                 (set! num (%f64-mul num ndigits)))
             ;; Apply sign
             (if (%i31-ne neg 0) (%f64-neg num) num))
           ;; Not a valid float → try complex or symbol
           ,try-complex-or-symbol)))

  ;; Read atom: consume token into mem[1024+], try integer parse, else float or symbol
  (define read-atom
    `(begin
       ;; Store first char and consume it
       (set! tok-len 0)
       (%mem-store8 1024 code)
       (set! tok-len 1)
       (%call ,fn-read-char port)
       ;; Read more chars until delimiter or EOF
       (%block-void
         (%loop-void
           (set! ch (%call ,fn-peek-char port))
           (%br-if 1 (%eof? ch))
           (set! code (%char-code ch))
           (%br-if 1 ,(delim? 'code))
           (%mem-store8 (%i31-add 1024 tok-len) code)
           (set! tok-len (%i31-add tok-len 1))
           (%call ,fn-read-char port)
           (%br 0)))
       ;; Try to parse as integer
       (set! i 0)
       (set! neg 0)
       (set! num 0)
       ;; Check for sign prefix (only if more than 1 char)
       (if (if (%i31-gt tok-len 1) (%i31-eq (%mem-load8 1024) 45) #f)
           (begin (set! neg 1) (set! i 1))
           (if (if (%i31-gt tok-len 1) (%i31-eq (%mem-load8 1024) 43) #f)
               (set! i 1)
               0))
       ;; Save digit start position (for rational check)
       (set! xval i)
       ;; Check if all remaining chars are digits
       (%block-void
         (%loop-void
           (%br-if 1 (%i31-ge i tok-len))
           (set! ch (%mem-load8 (%i31-add 1024 i)))
           (%br-if 1 (not (if (%i31-ge ch 48) (%i31-le ch 57) #f)))
           (set! num (%i31-add (%i31-mul num 10) (%i31-sub ch 48)))
           (set! i (%i31-add i 1))
           (%br 0)))
       ;; If loop completed (all digits), return number
       (if (%i31-ge i tok-len)
           (if (%i31-ne neg 0) (%i31-neg num) num)
           ;; Check for rational: slash after at least one digit (e.g., 1/3)
           (if (if (%i31-eq ch 47) (%i31-gt i xval) #f)
               ,try-rational
               ,try-float-or-symbol))))

  ;; Handle #\<char> literal
  (define read-char-literal
    `(begin
       (set! ch (%call ,fn-read-char port))
       (set! code (%char-code ch))
       ;; Read more lowercase letters for named chars
       (set! tok-len 0)
       (%mem-store8 1024 code)
       (set! tok-len 1)
       (%block-void
         (%loop-void
           (set! ch (%call ,fn-peek-char port))
           (%br-if 1 (%eof? ch))
           (set! i (%char-code ch))
           (%br-if 1 (not (if (%i31-ge i 97) (%i31-le i 122) #f)))
           (%mem-store8 (%i31-add 1024 tok-len) i)
           (set! tok-len (%i31-add tok-len 1))
           (%call ,fn-read-char port)
           (%br 0)))
       ;; Single char → char literal
       (if (%i31-eq tok-len 1)
           (%make-char (%mem-load8 1024))
           ;; Named: space(5,'s'=115), newline(7,'n'=110), tab(3,'t'=116), return(6,'r'=114)
           (if (if (%i31-eq tok-len 5) (%i31-eq (%mem-load8 1024) 115) #f)
               (%make-char 32)
               (if (if (%i31-eq tok-len 7) (%i31-eq (%mem-load8 1024) 110) #f)
                   (%make-char 10)
                   (if (if (%i31-eq tok-len 3) (%i31-eq (%mem-load8 1024) 116) #f)
                       (%make-char 9)
                       (if (if (%i31-eq tok-len 6) (%i31-eq (%mem-load8 1024) 114) #f)
                           (%make-char 13)
                           (%make-char (%mem-load8 1024)))))))))

  ;; Handle #x<hex-digits> hex literal
  (define read-hex-literal
    `(begin
       (%call ,fn-read-char port)  ;; consume 'x'
       (set! num 0)
       (%block-void
         (%loop-void
           (set! ch (%call ,fn-peek-char port))
           (%br-if 1 (%eof? ch))
           (set! code (%char-code ch))
           ;; Compute hex digit value: 0-9 → 0-9, a-f → 10-15, A-F → 10-15, else → 99
           (set! i
             (if (if (%i31-ge code 48) (%i31-le code 57) #f)
                 (%i31-sub code 48)
                 (if (if (%i31-ge code 97) (%i31-le code 102) #f)
                     (%i31-sub code 87)
                     (if (if (%i31-ge code 65) (%i31-le code 70) #f)
                         (%i31-sub code 55)
                         99))))
           ;; If not a valid hex digit, break
           (%br-if 1 (%i31-gt i 15))
           (set! num (%i31-add (%i31-mul num 16) i))
           (%call ,fn-read-char port)
           (%br 0)))
       num))

  ;; Handle # dispatch: #t, #f, #\<char>, #x<hex>, ##<symbol>
  (define hash-dispatch
    `(begin
       (%call ,fn-read-char port)
       (set! ch (%call ,fn-peek-char port))
       (if (%eof? ch)
           0
           (begin
             (set! code (%char-code ch))
             (if (%i31-eq code 116)
                 (begin (%call ,fn-read-char port) #t)
                 (if (%i31-eq code 102)
                     (begin (%call ,fn-read-char port) #f)
                     (if (%i31-eq code 92)
                         (begin (%call ,fn-read-char port) ,read-char-literal)
                         (if (%i31-eq code 120)
                             ,read-hex-literal
                             (if (%i31-eq code 35)
                                 ;; ## prefix: read rest as symbol with ## prepended
                                 (begin
                                   (%call ,fn-read-char port)
                                   (set! tok-len 0)
                                   (%mem-store8 1024 35)
                                   (%mem-store8 1025 35)
                                   (set! tok-len 2)
                                   (%block-void
                                     (%loop-void
                                       (set! ch (%call ,fn-peek-char port))
                                       (%br-if 1 (%eof? ch))
                                       (set! code (%char-code ch))
                                       (%br-if 1 ,(delim? 'code))
                                       (%mem-store8 (%i31-add 1024 tok-len) code)
                                       (set! tok-len (%i31-add tok-len 1))
                                       (%call ,fn-read-char port)
                                       (%br 0)))
                                   ,tok-to-symbol)
                                 0)))))))))

  ;; Handle |...| pipe-delimited symbols
  (define read-pipe-symbol
    `(begin
       (%call ,fn-read-char port)  ;; consume opening |
       (set! tok-len 0)
       (%block-void
         (%loop-void
           (set! ch (%call ,fn-read-char port))
           (%br-if 1 (%eof? ch))
           (set! code (%char-code ch))
           (%br-if 1 (%i31-eq code 124))  ;; closing |
           (%mem-store8 (%i31-add 1024 tok-len) code)
           (set! tok-len (%i31-add tok-len 1))
           (%br 0)))
       ,tok-to-symbol))

  ;; Handle , and ,@
  (define read-unquote
    `(begin
       (%call ,fn-read-char port)
       (set! ch (%call ,fn-peek-char port))
       (if (if (not (%eof? ch)) (%i31-eq (%char-code ch) 64) #f)
           (begin
             (%call ,fn-read-char port)
             ,(wrap-symbol "unquote-splicing"))
           ,(wrap-symbol "unquote"))))

  ;; Main body
  (list
    '("port")
    '("ch" "code" "tok-len" "i" "neg" "num" "fdigits" "xval" "xneg" "ndigits")
    (list
      `(begin
         (set! ch (%call ,fn-peek-char port))
         (if (%eof? ch)
             (%eof-new)
             (begin
               (set! code (%char-code ch))
               (if ,(ws? 'code)
                   ;; Whitespace: consume and recurse
                   (begin (%call ,fn-read-char port) (%call ,fn-read port))
                   (if (%i31-eq code 59)
                       ;; Comment: skip to newline
                       (begin
                         (%call ,fn-read-char port)
                         (%block-void
                           (%loop-void
                             (set! ch (%call ,fn-read-char port))
                             (%br-if 1 (%eof? ch))
                             (%br-if 1 (%i31-eq (%char-code ch) 10))
                             (%br 0)))
                         (%call ,fn-read port))
                       (if (%i31-eq code 40)
                           ;; Open paren
                           (begin (%call ,fn-read-char port)
                                  (%call ,fn-read-list port))
                           (if (%i31-eq code 41)
                               ;; Close paren (unexpected)
                               (begin (%call ,fn-read-char port) 0)
                               (if (%i31-eq code 34)
                                   ;; String
                                   (begin (%call ,fn-read-char port)
                                          (%call ,fn-read-string port))
                                   (if (%i31-eq code 35)
                                       ;; Hash
                                       ,hash-dispatch
                                       (if (%i31-eq code 39)
                                           ;; Quote
                                           (begin (%call ,fn-read-char port)
                                                  ,(wrap-symbol "quote"))
                                           (if (%i31-eq code 96)
                                               ;; Quasiquote
                                               (begin (%call ,fn-read-char port)
                                                      ,(wrap-symbol "quasiquote"))
                                               (if (%i31-eq code 44)
                                                   ;; Unquote
                                                   ,read-unquote
                                                   (if (%i31-eq code 124)
                                                       ;; Pipe-delimited symbol
                                                       ,read-pipe-symbol
                                                       ;; Atom
                                                       ,read-atom))))))))))))))))

;; make-rt-read-list: (port) → list
;; Reads list elements until ')'. Handles dotted pairs.
(define (make-rt-read-list fn-read fn-read-list fn-peek-char fn-read-char)
  ;; helper: test if code is a delimiter
  (define (delim? c)
    `(if (%i31-eq ,c 32) #t
         (if (%i31-eq ,c 10) #t
             (if (%i31-eq ,c 9) #t
                 (if (%i31-eq ,c 13) #t
                     (if (%i31-eq ,c 40) #t
                         (if (%i31-eq ,c 41) #t
                             (if (%i31-eq ,c 34) #t
                                 (%i31-eq ,c 59)))))))))

  ;; Skip whitespace and comments helper (inline in body)
  (define skip-ws
    `(%block-void
       (%loop-void
         (set! ch (%call ,fn-peek-char port))
         (%br-if 1 (%eof? ch))
         (set! code (%char-code ch))
         (if (if (%i31-eq code 32) #t
                 (if (%i31-eq code 10) #t
                     (if (%i31-eq code 9) #t
                         (%i31-eq code 13))))
             (begin (%call ,fn-read-char port) (%br 1))
             0)
         (if (%i31-eq code 59)
             (begin
               (%call ,fn-read-char port)
               (%block-void
                 (%loop-void
                   (set! ch (%call ,fn-read-char port))
                   (%br-if 1 (%eof? ch))
                   (%br-if 1 (%i31-eq (%char-code ch) 10))
                   (%br 0)))
               (%br 1))
             0)
         (%br 1))))

  (list
    '("port")
    '("ch" "code" "datum" "tmp" "tok-len" "i")
    (list
      `(begin
         ;; Skip whitespace
         ,skip-ws
         ;; Check for ) or EOF
         (set! ch (%call ,fn-peek-char port))
         (if (%eof? ch)
             (%ref-null)
             (begin
               (set! code (%char-code ch))
               (if (%i31-eq code 41)
                   ;; Close paren
                   (begin (%call ,fn-read-char port) (%ref-null))
                   (begin
                     ;; Read next datum
                     (set! datum (%call ,fn-read port))
                     ;; Skip whitespace
                     ,skip-ws
                     ;; Check for dotted pair
                     (set! ch (%call ,fn-peek-char port))
                     (if (if (not (%eof? ch)) (%i31-eq (%char-code ch) 46) #f)
                         ;; Peeked a dot — consume it and check what follows
                         (begin
                           (%call ,fn-read-char port)
                           (set! ch (%call ,fn-peek-char port))
                           (set! code (if (%eof? ch) 32 (%char-code ch)))
                           (if ,(delim? 'code)
                               ;; Standalone dot → dotted pair
                               (begin
                                 (set! tmp (%call ,fn-read port))
                                 ,skip-ws
                                 (set! ch (%call ,fn-peek-char port))
                                 (if (if (not (%eof? ch)) (%i31-eq (%char-code ch) 41) #f)
                                     (%call ,fn-read-char port)
                                     0)
                                 (cons datum tmp))
                               ;; Dot followed by non-delimiter → token like ... or ..
                               ;; Build token in memory buffer, convert to symbol
                               (begin
                                 (%mem-store8 1024 46)
                                 (set! tok-len 1)
                                 (set! ch (%call ,fn-read-char port))
                                 (%mem-store8 (%i31-add 1024 tok-len) (%char-code ch))
                                 (set! tok-len (%i31-add tok-len 1))
                                 (%block-void
                                   (%loop-void
                                     (set! ch (%call ,fn-peek-char port))
                                     (%br-if 1 (%eof? ch))
                                     (set! code (%char-code ch))
                                     (%br-if 1 ,(delim? 'code))
                                     (%mem-store8 (%i31-add 1024 tok-len) code)
                                     (set! tok-len (%i31-add tok-len 1))
                                     (%call ,fn-read-char port)
                                     (%br 0)))
                                 (set! tmp (%make-string tok-len 0))
                                 (set! i 0)
                                 (%block-void
                                   (%loop-void
                                     (%br-if 1 (%i31-ge i tok-len))
                                     (%string-set! tmp i (%mem-load8 (%i31-add 1024 i)))
                                     (set! i (%i31-add i 1))
                                     (%br 0)))
                                 (cons datum (cons (string->symbol tmp)
                                                   (%call ,fn-read-list port))))))
                         ;; Regular: (cons datum (read-list port))
                         (cons datum (%call ,fn-read-list port)))))))))))

;; make-rt-read-string: (port) → string
;; Reads characters until closing quote. Handles escape sequences.
(define (make-rt-read-string fn-read-char)
  (list
    '("port")
    '("ch" "code" "tok-len" "arr" "i")
    (list
      `(begin
         (set! tok-len 0)
         ;; Read chars into mem[1024+] until closing "
         (%block-void
           (%loop-void
             (set! ch (%call ,fn-read-char port))
             (%br-if 1 (%eof? ch))
             (set! code (%char-code ch))
             (%br-if 1 (%i31-eq code 34))
             ;; Escape or normal char
             (if (%i31-eq code 92)
                 ;; Escape sequence
                 (begin
                   (set! ch (%call ,fn-read-char port))
                   (set! code (%char-code ch))
                   (%mem-store8 (%i31-add 1024 tok-len)
                     (if (%i31-eq code 110) 10
                         (if (%i31-eq code 116) 9
                             (if (%i31-eq code 34) 34
                                 (if (%i31-eq code 92) 92
                                     code))))))
                 ;; Normal char
                 (%mem-store8 (%i31-add 1024 tok-len) code))
             (set! tok-len (%i31-add tok-len 1))
             (%br 0)))
         ;; Build GC string from buffer
         (set! arr (%make-string tok-len 0))
         (set! i 0)
         (%block-void
           (%loop-void
             (%br-if 1 (%i31-ge i tok-len))
             (%string-set! arr i (%mem-load8 (%i31-add 1024 i)))
             (set! i (%i31-add i 1))
             (%br 0)))
         arr))))
