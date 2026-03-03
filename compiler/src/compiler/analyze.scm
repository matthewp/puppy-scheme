;;; analyze.scm — Analysis phase: scan builtins, free variables, lambda lifting
;;; Operates on native Scheme data (lists, symbols) before marshaling.

;; WIT world for Component Model support (set by main.scm before analyze)
(define *wit-world* #f)

;;; --- Simple string hash table (64-bucket, chained) ---
;;; Used by analyze, codegen-expr, and codegen for O(1) string lookups.

(define (string-hash s)
  (let ((len (string-length s)))
    (let loop ((i 0) (h 0))
      (if (>= i len)
          (bitwise-and h 63)
          (loop (+ i 1)
                (bitwise-and (+ h (char->integer (string-ref s i)))
                             #x3FFFFFFF))))))

(define (make-string-ht)
  (make-vector 64 '()))

(define (string-ht-set! ht key val)
  (let ((idx (string-hash key)))
    (vector-set! ht idx (cons (cons key val) (vector-ref ht idx)))))

(define (string-ht-ref ht key default)
  (let ((idx (string-hash key)))
    (let loop ((b (vector-ref ht idx)))
      (cond
        ((null? b) default)
        ((string=? (caar b) key) (cdar b))
        (else (loop (cdr b)))))))

(define (string-ht-has? ht key)
  (let ((idx (string-hash key)))
    (let loop ((b (vector-ref ht idx)))
      (cond
        ((null? b) #f)
        ((string=? (caar b) key) #t)
        (else (loop (cdr b)))))))

(define (wit-export-or-import-name? name)
  (and *wit-world*
       (let loop ((items (wit-world-items *wit-world*)))
         (cond
           ((null? items) #f)
           ((and (pair? (car items))
                 (or (eq? (car (car items)) 'export)
                     (eq? (car (car items)) 'import))
                 (= (length (car items)) 3)
                 (string=? (cadr (car items)) name))
            #t)
           (else (loop (cdr items)))))))

;;; --- Growable list (O(1) append via tail pointer) ---
;;; glist is a 3-element vector: #(head tail count)
;;; head and tail are list cells; tail points to last cons cell.

(define (make-glist) (vector '() '() 0))

(define (glist-count gl) (vector-ref gl 2))
(define (glist-head gl) (vector-ref gl 0))

(define (glist-add! gl item)
  (let ((new-cell (list item)))
    (if (null? (vector-ref gl 0))
        (begin (vector-set! gl 0 new-cell)
               (vector-set! gl 1 new-cell))
        (begin (set-cdr! (vector-ref gl 1) new-cell)
               (vector-set! gl 1 new-cell)))
    (vector-set! gl 2 (+ (vector-ref gl 2) 1))))

;;; --- Primitives list (ported from closure.c) ---

(define *primitives*
  '("+" "-" "*" "/" "=" "<" ">" "<=" ">="
    "display" "newline" "not" "boolean?" "eq?" "eqv?" "equal?"
    "number?" "integer?" "exact?" "inexact?" "flonum?"
    "zero?" "positive?" "negative?" "odd?" "even?"
    "abs" "quotient" "remainder" "modulo"
    "max" "min"
    "gcd" "lcm"
    "floor" "ceiling" "truncate" "round"
    "exact->inexact" "inexact->exact"
    "sqrt" "exp" "log" "sin" "cos" "tan"
    "asin" "acos" "atan" "expt"
    "number->string" "string->number"
    "cons" "car" "cdr" "null?" "pair?"
    "if" "define" "lambda" "begin" "set!" "quote"
    "let" "let*" "letrec" "cond" "case" "do" "and" "or" "else"
    "quasiquote" "unquote" "unquote-splicing"
    "define-syntax" "syntax-rules"
    "let-syntax" "letrec-syntax"
    "make-bytevector" "bytevector-length"
    "bytevector-u8-ref" "bytevector-u8-set!"
    "bytevector-u32-native-ref" "bytevector-u32-native-set!"
    "bytevector-f64-native-set!" "##flonum->ieee754-64"
    "bytevector->pointer" "bytevector-copy-string!"
    "bytevector-copy!" "write-bytevector"
    "char?" "char=?" "char<?" "char>?" "char<=?" "char>=?"
    "char-alphabetic?" "char-numeric?" "char-whitespace?"
    "char-upcase" "char-downcase"
    "char-ci=?" "char-ci<?" "char-ci>?" "char-ci<=?" "char-ci>=?"
    "symbol?" "symbol->string" "string->symbol"
    "procedure?"
    "list" "pointer->string" "apply"
    "linear-alloc"
    "rational?" "##ratnum?" "##ratnum-numerator" "##ratnum-denominator"
    "##cpxnum?" "##cpxnum-real" "##cpxnum-imag"
    "make-rectangular" "real-part" "imag-part" "complex?"
    "command-line" "string-append"
    "file-exists?" "open-input-file" "open-output-file"
    "close-input-port" "close-output-port"
    "read-char" "peek-char" "write-char"
    "eof-object?" "port?" "input-port?" "output-port?"
    "call-with-input-file" "call-with-output-file"
    "open-input-string"
    "string?" "string-length" "string-ref" "string-set!" "make-string"
    "substring" "string-copy" "string->list" "list->string" "string=?"
    "read" "char->integer" "integer->char"
    "vector" "vector-ref" "vector-set!" "vector-length" "make-vector"
    "vector-copy" "list->vector" "vector->list" "vector?"
    "bitwise-and" "bitwise-ior" "arithmetic-shift"
    "set-car!" "set-cdr!"
    "delay" "force" "promise?"
    "%make-promise" "%promise-ref" "%promise-set!"
    "%promise-state" "%promise-set-state!"
    "for-each" "append"
    "length" "map" "assq" "list-ref" "reverse" "list?" "memq" "memv" "member" "filter"
    "write" "exit"
    "current-error-port" "current-milliseconds"
    "caar" "cadr" "cdar" "cddr"
    "caaar" "caadr" "cadar" "caddr" "cdaar" "cdadr" "cddar" "cdddr"
    "caaaar" "caaadr" "caadar" "caaddr" "cadaar" "cadadr" "caddar" "cadddr"
    "cdaaar" "cdaadr" "cdadar" "cdaddr" "cddaar" "cddadr" "cdddar" "cddddr"))

;; Use string=? instead of equal? for member checks — equal?'s %string?
;; type test can fail under WASM GC due to ref.test on nominal types
(define (string-member name lst)
  (let loop ((ls lst))
    (cond
      ((null? ls) #f)
      ((not (string? (car ls)))
       (loop (cdr ls)))
      ((not (string? name)) #f)
      ((string=? (car ls) name) ls)
      (else (loop (cdr ls))))))

(define *primitives-ht*
  (let ((ht (make-string-ht)))
    (let loop ((ps *primitives*))
      (when (pair? ps)
        (string-ht-set! ht (car ps) #t)
        (loop (cdr ps))))
    ht))

(define (primitive? name)
  (string-ht-has? *primitives-ht* name))

;;; --- Builtin flags and index-map ---

(define FLAG-DISPLAY 0)
(define FLAG-NEWLINE 1)
(define FLAG-EQV 2)
(define FLAG-EQUAL 3)
(define FLAG-NUM-TO-STR 4)
(define FLAG-STR-TO-NUM 5)
(define FLAG-FLONUM 6)
(define FLAG-RATIONAL 7)
(define FLAG-COMPLEX 8)
(define FLAG-MATH 9)
(define FLAG-COMMAND-LINE 10)
(define FLAG-STRING-APPEND 11)
(define FLAG-BYTEVECTOR 12)
(define FLAG-CHAR 13)
(define FLAG-SYMBOL 14)
(define FLAG-FILE-IO 15)
(define FLAG-STRING-OPS 16)
(define FLAG-READ 17)
(define FLAG-VECTOR 18)
(define FLAG-WRITE 19)
(define FLAG-CLOCK 20)
(define FLAG-APPLY 21)
(define FLAG-FILE-EXISTS 22)
(define FLAG-BV-COPY 23)
(define FLAG-BV-APPEND 24)
(define FLAG-UTF8-STRING 25)
(define FLAG-GET-ENV 26)
(define FLAG-EXIT 27)
(define FLAG-PROMISE 28)
(define FLAG-COUNT 29)

(define (make-flags) (make-vector FLAG-COUNT #f))

;;; --- Index-map constants ---
;;; Positions in the index-map vector returned by compute-index-map.
;;; Contains all function indices, type indices, and derived booleans.

;; Import function indices
(define IDX-FN-PROC-EXIT 0)
;; Builtin function indices
(define IDX-FN-DISPLAY 8)
(define IDX-FN-WRITE 9)
(define IDX-FN-NEWLINE 10)
(define IDX-FN-EQV 11)
(define IDX-FN-EQUAL 12)
(define IDX-FN-NUM-TO-STR 13)
(define IDX-FN-STR-TO-NUM 14)
(define IDX-FN-FLONUM-START 15)
(define IDX-FN-GCD 16)
(define IDX-FN-MAKE-COMPLEX 17)
(define IDX-FN-MATH-START 18)
(define IDX-FN-COMMAND-LINE 19)
(define IDX-FN-STRING-APPEND 20)
(define IDX-FN-LINEAR-ALLOC 21)
(define IDX-FN-BV-ALLOC 22)
(define IDX-FN-BV-COPY-STR 23)
(define IDX-FN-PTR-TO-STR 24)
(define IDX-FN-BV-ALLOC-FILL 25)
(define IDX-FN-OPEN-INPUT-FILE 26)
(define IDX-FN-OPEN-OUTPUT-FILE 27)
(define IDX-FN-CLOSE-PORT 28)
(define IDX-FN-READ-CHAR 29)
(define IDX-FN-PEEK-CHAR 30)
(define IDX-FN-WRITE-CHAR 31)
(define IDX-FN-SUBSTRING 32)
(define IDX-FN-STRING-COPY 33)
(define IDX-FN-STRING-TO-LIST 34)
(define IDX-FN-LIST-TO-STRING 35)
(define IDX-FN-STRING-EQ 36)
(define IDX-FN-STRING-LT 37)
(define IDX-FN-STRING-CI-EQ 38)
(define IDX-FN-STRING-CI-LT 39)
(define IDX-FN-READ 40)
(define IDX-FN-READ-LIST 41)
(define IDX-FN-READ-STRING 42)
(define IDX-FN-VECTOR-COPY 43)
(define IDX-FN-LIST-TO-VECTOR 44)
(define IDX-FN-VECTOR-TO-LIST 45)
(define IDX-FN-INTERN-SYM 46)
(define IDX-FN-APPLY 47)
(define IDX-FN-FILE-EXISTS 48)
(define IDX-FN-USER-START 49)
;; Type indices
(define IDX-TY-CHAR 50)
(define IDX-TY-EQV 51)
(define IDX-TY-SYMBOL 52)
(define IDX-TY-NUMSTR 53)
(define IDX-TY-FLONUM 54)
(define IDX-TY-RATIONAL 55)
(define IDX-TY-COMPLEX 56)
(define IDX-TY-ARGS-IMPORT 57)
(define IDX-TY-VOID-EQREF 58)
(define IDX-TY-BYTEVECTOR 59)
(define IDX-TY-BV-COPY-STR 60)
(define IDX-TY-PORT 61)
(define IDX-TY-EOF 62)
(define IDX-TY-STR-3ARG 65)
(define IDX-TY-CLOCK-IMPORT 66)
(define IDX-TY-VECTOR 67)
(define IDX-TY-USER-START 68)
;; Counts
(define IDX-NIMPORTS 69)
(define IDX-NBUILTINS 70)
;; Derived booleans
(define IDX-NEEDS-DISPATCH 71)
(define IDX-NEEDS-EQV-TYPE 72)
(define IDX-NEEDS-IO 73)
(define IDX-NEEDS-MEMORY 74)
(define IDX-NEEDS-NUMSTR 75)
(define IDX-FN-BV-COPY-RANGE 76)
(define IDX-FN-BV-COPY 77)
(define IDX-FN-BV-COPY-FROM 78)
(define IDX-FN-BV-APPEND 79)
(define IDX-FN-UTF8-TO-STR-RANGE 80)
(define IDX-FN-UTF8-TO-STR 81)
(define IDX-FN-UTF8-TO-STR-FROM 82)
(define IDX-FN-STR-TO-UTF8-RANGE 83)
(define IDX-FN-STR-TO-UTF8 84)
(define IDX-FN-STR-TO-UTF8-FROM 85)
;; environ builtin indices
(define IDX-FN-GET-ENV-VAR 88)
(define IDX-FN-GET-ENV-VARS 89)
(define IDX-FN-OPEN-INPUT-STRING 90)
(define IDX-WIT-NIMPORTS 91)
;; WASI P2 import function indices
(define IDX-FN-GET-STDOUT 92)
(define IDX-FN-GET-STDERR 93)
(define IDX-FN-STREAM-WRITE 94)
(define IDX-TY-VOID-I32 95) ;; ()->i32 type for get-stdout/get-stderr
(define IDX-NEEDS-EXIT 97)
(define IDX-FN-CLOCK-NOW 98) ;; P2 clock-now import index
(define IDX-TY-VOID-I64 99) ;; ()->i64 type for clock-now
(define IDX-FN-GET-ARGUMENTS 100) ;; P2 get-arguments import index
(define IDX-FN-GET-ENVIRONMENT 101) ;; P2 get-environment import index
;; P2 file I/O import indices
(define IDX-FN-GET-STDIN 102)
(define IDX-FN-STREAM-READ 103)
(define IDX-FN-GET-DIRECTORIES 104)
(define IDX-FN-OPEN-AT 105)
(define IDX-FN-READ-VIA-STREAM 106)
(define IDX-FN-WRITE-VIA-STREAM 107)
(define IDX-FN-DROP-DESCRIPTOR 108)
(define IDX-FN-DROP-INPUT-STREAM 109)
(define IDX-FN-DROP-OUTPUT-STREAM 110)
(define IDX-TY-STREAM-READ 111)  ;; (i32, i64, i32) -> ()
(define IDX-TY-OPEN-AT 112)     ;; (i32*7) -> ()

(define IDX-FN-VECTOR-FILL 113)
(define IDX-FN-STRING-FILL 114)
(define IDX-TY-PROMISE 115)
(define IDX-MAP-SIZE 116)

;; Hash table mapping builtin names to their flag index for O(1) scan
(define *scan-ht*
  (let ((ht (make-string-ht)))
    (for-each (lambda (pair) (string-ht-set! ht (car pair) (cdr pair)))
      (list
        (cons "newline" FLAG-NEWLINE)
        (cons "eqv?" FLAG-EQV)
        (cons "equal?" FLAG-EQUAL)
        (cons "number->string" FLAG-NUM-TO-STR)
        (cons "string->number" FLAG-STR-TO-NUM)
        (cons "exact->inexact" FLAG-FLONUM)
        (cons "flonum?" FLAG-FLONUM)
        (cons "rational?" FLAG-RATIONAL)
        (cons "##ratnum?" FLAG-RATIONAL)
        (cons "##ratnum-numerator" FLAG-RATIONAL)
        (cons "##ratnum-denominator" FLAG-RATIONAL)
        (cons "##cpxnum?" FLAG-COMPLEX)
        (cons "##cpxnum-real" FLAG-COMPLEX)
        (cons "##cpxnum-imag" FLAG-COMPLEX)
        (cons "make-rectangular" FLAG-COMPLEX)
        (cons "real-part" FLAG-COMPLEX)
        (cons "imag-part" FLAG-COMPLEX)
        (cons "complex?" FLAG-COMPLEX)
        (cons "sqrt" FLAG-MATH) (cons "exp" FLAG-MATH)
        (cons "log" FLAG-MATH) (cons "sin" FLAG-MATH)
        (cons "cos" FLAG-MATH) (cons "tan" FLAG-MATH)
        (cons "asin" FLAG-MATH) (cons "acos" FLAG-MATH)
        (cons "atan" FLAG-MATH) (cons "expt" FLAG-MATH)
        (cons "command-line" FLAG-COMMAND-LINE)
        (cons "string-append" FLAG-STRING-APPEND)
        (cons "make-bytevector" FLAG-BYTEVECTOR)
        (cons "bytevector-length" FLAG-BYTEVECTOR)
        (cons "bytevector-u8-ref" FLAG-BYTEVECTOR)
        (cons "bytevector-u8-set!" FLAG-BYTEVECTOR)
        (cons "bytevector-u32-native-ref" FLAG-BYTEVECTOR)
        (cons "bytevector-u32-native-set!" FLAG-BYTEVECTOR)
        (cons "bytevector-f64-native-set!" FLAG-BYTEVECTOR)
        (cons "bytevector->pointer" FLAG-BYTEVECTOR)
        (cons "bytevector-copy-string!" FLAG-BYTEVECTOR)
        (cons "bytevector-copy!" FLAG-BYTEVECTOR)
        (cons "bytevector?" FLAG-BYTEVECTOR)
        (cons "bytevector-copy" FLAG-BV-COPY)
        (cons "bytevector-append" FLAG-BV-APPEND)
        (cons "utf8->string" FLAG-UTF8-STRING)
        (cons "string->utf8" FLAG-UTF8-STRING)
        (cons "pointer->string" FLAG-BYTEVECTOR)
        (cons "linear-alloc" FLAG-BYTEVECTOR)
        (cons "char?" FLAG-CHAR)
        (cons "char<?" FLAG-CHAR)
        (cons "char>?" FLAG-CHAR)
        (cons "char<=?" FLAG-CHAR)
        (cons "char>=?" FLAG-CHAR)
        (cons "char-alphabetic?" FLAG-CHAR)
        (cons "char-numeric?" FLAG-CHAR)
        (cons "char-whitespace?" FLAG-CHAR)
        (cons "char-upcase" FLAG-CHAR)
        (cons "char-downcase" FLAG-CHAR)
        (cons "char-ci=?" FLAG-CHAR)
        (cons "char-ci<?" FLAG-CHAR)
        (cons "char-ci>?" FLAG-CHAR)
        (cons "char-ci<=?" FLAG-CHAR)
        (cons "char-ci>=?" FLAG-CHAR)
        (cons "symbol?" FLAG-SYMBOL)
        (cons "symbol->string" FLAG-SYMBOL)
        (cons "string->symbol" FLAG-SYMBOL)
        (cons "open-input-file" FLAG-FILE-IO)
        (cons "open-output-file" FLAG-FILE-IO)
        (cons "close-input-port" FLAG-FILE-IO)
        (cons "close-output-port" FLAG-FILE-IO)
        (cons "read-char" FLAG-FILE-IO)
        (cons "peek-char" FLAG-FILE-IO)
        (cons "write-char" FLAG-FILE-IO)
        (cons "eof-object?" FLAG-FILE-IO)
        (cons "port?" FLAG-FILE-IO)
        (cons "input-port?" FLAG-FILE-IO)
        (cons "output-port?" FLAG-FILE-IO)
        (cons "current-error-port" FLAG-DISPLAY)
        (cons "string?" FLAG-STRING-OPS)
        (cons "string-length" FLAG-STRING-OPS)
        (cons "string-ref" FLAG-STRING-OPS)
        (cons "string-set!" FLAG-STRING-OPS)
        (cons "make-string" FLAG-STRING-OPS)
        (cons "substring" FLAG-STRING-OPS)
        (cons "string-copy" FLAG-STRING-OPS)
        (cons "string->list" FLAG-STRING-OPS)
        (cons "list->string" FLAG-STRING-OPS)
        (cons "string=?" FLAG-STRING-OPS)
        (cons "string<?" FLAG-STRING-OPS)
        (cons "string>?" FLAG-STRING-OPS)
        (cons "string<=?" FLAG-STRING-OPS)
        (cons "string>=?" FLAG-STRING-OPS)
        (cons "string-ci=?" FLAG-STRING-OPS)
        (cons "string-ci<?" FLAG-STRING-OPS)
        (cons "string-ci>?" FLAG-STRING-OPS)
        (cons "string-ci<=?" FLAG-STRING-OPS)
        (cons "string-ci>=?" FLAG-STRING-OPS)
        (cons "string-fill!" FLAG-STRING-OPS)
        (cons "current-milliseconds" FLAG-CLOCK)
        (cons "read" FLAG-READ)
        (cons "vector" FLAG-VECTOR)
        (cons "vector-ref" FLAG-VECTOR)
        (cons "vector-set!" FLAG-VECTOR)
        (cons "vector-length" FLAG-VECTOR)
        (cons "make-vector" FLAG-VECTOR)
        (cons "vector-copy" FLAG-VECTOR)
        (cons "list->vector" FLAG-VECTOR)
        (cons "vector->list" FLAG-VECTOR)
        (cons "vector?" FLAG-VECTOR)
        (cons "vector-fill!" FLAG-VECTOR)
        (cons "apply" FLAG-APPLY)
        (cons "file-exists?" FLAG-FILE-EXISTS)
        (cons "get-environment-variable" FLAG-GET-ENV)
        (cons "get-environment-variables" FLAG-GET-ENV)
        (cons "open-input-string" FLAG-FILE-IO)
        (cons "exit" FLAG-EXIT)
        (cons "emergency-exit" FLAG-EXIT)
        (cons "%make-promise" FLAG-PROMISE)
        (cons "%promise-ref" FLAG-PROMISE)
        (cons "%promise-set!" FLAG-PROMISE)
        (cons "%promise-state" FLAG-PROMISE)
        (cons "%promise-set-state!" FLAG-PROMISE)
        (cons "promise?" FLAG-PROMISE)))
    ht))

;;; --- Builtin dependency table ---
;;; Maps builtin flag names to their transitive dependencies.
;;; Derived from the when chains in analyze-forms and codegen-module.

(define *flag-names*
  ;; Parallel to FLAG-* constants: index i → name string
  (vector "display" "newline" "eqv" "equal" "num-to-str" "str-to-num"
          "flonum" "rational" "complex" "math" "command-line" "string-append"
          "bytevector" "char" "symbol" "file-io" "string-ops" "read" "vector"
          "write" "clock" "apply" "file-exists" "bv-copy" "bv-append" "utf8-string" "get-env"
          "exit" "promise"))

(define *flag-name-ht*
  ;; name string → FLAG index
  (let ((ht (make-string-ht)))
    (let loop ((i 0))
      (when (< i FLAG-COUNT)
        (string-ht-set! ht (vector-ref *flag-names* i) i)
        (loop (+ i 1))))
    ht))

(define *builtin-deps*
  ;; (name dep1 dep2 ...) — if name is reachable, deps also become reachable
  '(("display" "char" "symbol")
    ("write" "char" "symbol")
    ("read" "file-io" "char" "symbol" "flonum" "rational" "complex")
    ("math" "flonum")
    ("clock" "flonum")
    ("eqv" "char")
    ("equal" "char")
    ("file-io" "char")
    ("string-ops" "char")
    ("file-exists" "file-io")
    ("bv-copy" "bytevector")
    ("bv-append" "bytevector" "bv-copy")
    ("utf8-string" "bytevector")
    ("get-env" "string-ops")))

;;; --- find-free-vars (ported from closure.c:55-163) ---

(define (in-set? set name)
  (string-member name set))

(define (bound-set-extend bound names)
  ;; Extend a hash-table-based bound set with a list of name strings
  (let ((ht (vector-copy bound)))
    (for-each (lambda (n) (string-ht-set! ht n #t)) names)
    ht))

(define (find-free-vars expr bound globals)
  ;; Returns a list of free variable name strings (deduplicated)
  ;; bound and globals are lists of name strings
  (let ((result '())
        (result-ht (make-string-ht)))
    (define (fv-add name)
      (unless (string-ht-has? result-ht name)
        (string-ht-set! result-ht name #t)
        (set! result (cons name result))))
    (define (find-free expr bound)
      (cond
        ((symbol? expr)
         (let ((s (symbol->string expr)))
           (when (and (not (primitive? s))
                      (not (in-set? bound s))
                      (not (in-set? globals s)))
             (fv-add s))))

        ((not (pair? expr)) #f)

        ((null? expr) #f)

        (else
         (let ((head (car expr)))
           (cond
             ;; (lambda (params...) body...)
             ((and (symbol? head) (string=? (symbol->string head) "lambda")
                   (>= (length expr) 3) (list? (cadr expr)))
              (let* ((params (cadr expr))
                     (pnames (map (lambda (p) (symbol->string p)) params))
                     (new-bound (append pnames bound)))
                (for-each (lambda (b) (find-free b new-bound)) (cddr expr))))

             ;; (let ((x e) ...) body...)
             ((and (symbol? head) (string=? (symbol->string head) "let")
                   (>= (length expr) 3) (list? (cadr expr)))
              (let* ((bindings (cadr expr))
                     (bnames (map (lambda (b)
                                   (if (and (pair? b) (>= (length b) 1) (symbol? (car b)))
                                       (symbol->string (car b))
                                       "_"))
                                 bindings)))
                ;; init exprs in outer scope
                (for-each
                 (lambda (b)
                   (when (and (pair? b) (= (length b) 2))
                     (find-free (cadr b) bound)))
                 bindings)
                ;; body in extended scope
                (let ((new-bound (append bnames bound)))
                  (for-each (lambda (b) (find-free b new-bound)) (cddr expr)))))

             ;; (let* ((x e) ...) body...)
             ((and (symbol? head) (string=? (symbol->string head) "let*")
                   (>= (length expr) 3) (list? (cadr expr)))
              (let* ((bindings (cadr expr))
                     (bnames (map (lambda (b)
                                   (if (and (pair? b) (>= (length b) 1) (symbol? (car b)))
                                       (symbol->string (car b))
                                       "_"))
                                 bindings)))
                ;; each binding sees previous ones
                (let loop ((bs bindings) (bn bnames) (cur-bound bound))
                  (when (pair? bs)
                    (when (and (pair? (car bs)) (= (length (car bs)) 2))
                      (find-free (cadar bs) cur-bound))
                    (loop (cdr bs) (cdr bn) (cons (car bn) cur-bound))))
                ;; body sees all bindings
                (for-each (lambda (b) (find-free b (append bnames bound))) (cddr expr))))

             ;; (define name expr)
             ((and (symbol? head) (string=? (symbol->string head) "define"))
              (cond
                ;; (define name expr) where name is symbol
                ((and (>= (length expr) 3) (symbol? (cadr expr)))
                 (find-free (caddr expr) bound))
                ;; (define (name params...) body...)
                ((and (>= (length expr) 3) (pair? (cadr expr)))
                 (let* ((sig (cadr expr))
                        (pnames (map (lambda (p) (symbol->string p)) (cdr sig)))
                        (new-bound (append pnames bound)))
                   (for-each (lambda (b) (find-free b new-bound)) (cddr expr))))))

             ;; (quote ...) — skip quoted data
             ((and (symbol? head) (string=? (symbol->string head) "quote"))
              #f)

             ;; default: recurse
             (else
              (for-each (lambda (e) (find-free e bound)) expr)))))))

    (find-free expr bound)
    (reverse result)))

;;; --- find-set-targets: collect names of all set! target variables ---

(define (find-set-targets form)
  (let ((result '())
        (seen (make-string-ht)))
    (define (walk expr)
      (when (pair? expr)
        (let ((head (car expr)))
          (cond
            ((and (symbol? head)
                  (string=? (symbol->string head) "set!")
                  (>= (length expr) 3) (symbol? (cadr expr)))
             (let ((name (symbol->string (cadr expr))))
               (unless (string-ht-has? seen name)
                 (string-ht-set! seen name #t)
                 (set! result (cons name result))))
             (walk (caddr expr)))
            ((and (symbol? head)
                  (string=? (symbol->string head) "quote")) #f)
            (else (for-each walk expr))))))
    (walk form)
    result))

(define (find-set-targets-all forms)
  (let ((result '())
        (seen (make-string-ht)))
    (for-each
     (lambda (form)
       (for-each
        (lambda (name)
          (unless (string-ht-has? seen name)
            (string-ht-set! seen name #t)
            (set! result (cons name result))))
        (find-set-targets form)))
     forms)
    result))

;;; --- Form classification helpers (matching codegen_internal.h) ---

(define (sym=? x s)
  (and (symbol? x) (string=? (symbol->string x) s)))

(define (is-func-define-sugar? form)
  ;; (define (name params...) body...)
  (and (pair? form) (>= (length form) 3)
       (sym=? (car form) "define")
       (pair? (cadr form))
       (>= (length (cadr form)) 1)
       (symbol? (caadr form))))

(define (is-func-define-lambda? form)
  ;; (define name (lambda (params...) body...))
  (and (pair? form) (= (length form) 3)
       (sym=? (car form) "define")
       (symbol? (cadr form))
       (let ((e (caddr form)))
         (and (pair? e) (>= (length e) 3)
              (sym=? (car e) "lambda")
              (list? (cadr e))))))

(define (is-func-define? form)
  (or (is-func-define-sugar? form) (is-func-define-lambda? form)))

(define (is-var-define? form)
  ;; (define name expr) where expr is NOT a lambda
  (and (pair? form) (= (length form) 3)
       (sym=? (car form) "define")
       (symbol? (cadr form))
       (not (is-func-define-lambda? form))))

(define (is-external-define? form)
  ;; (define-external (name (type param)...) return-type body...)
  (and (pair? form) (>= (length form) 4)
       (sym=? (car form) "define-external")
       (pair? (cadr form))
       (>= (length (cadr form)) 1)
       (symbol? (caadr form))
       (symbol? (caddr form))))

;;; --- Index computation constants ---

(define TY_FIXED_COUNT 8)
(define FL_COUNT 15)
(define MATH_COUNT 11)

;;; --- Arity tracking ---

(define (find-or-add-arity! arities arity)
  ;; arities is a vector: #(arity0 arity1 ... count)
  ;; Returns position index (0, 1, 2, ...)
  ;; Final WASM type index = ty-user-start + position, assigned later.
  (let ((count (vector-ref arities 64)))
    (let loop ((j 0))
      (cond
        ((>= j count)
         (vector-set! arities count arity)
         (vector-set! arities 64 (+ count 1))
         count)
        ((= (vector-ref arities j) arity)
         j)
        (else (loop (+ j 1)))))))

;;; --- Data structure accessors ---
;;; user-func: #(name func-idx type-idx nparams param-names body-forms nbody parent-form body-offset)
(define (make-user-func name func-idx type-idx nparams param-names body-forms nbody parent-form body-offset)
  (vector name func-idx type-idx nparams param-names body-forms nbody parent-form body-offset))
(define (uf-name uf) (vector-ref uf 0))
(define (uf-func-idx uf) (vector-ref uf 1))
(define (uf-set-func-idx! uf v) (vector-set! uf 1 v))
(define (uf-type-idx uf) (vector-ref uf 2))
(define (uf-set-type-idx! uf v) (vector-set! uf 2 v))
(define (uf-nparams uf) (vector-ref uf 3))
(define (uf-set-nparams! uf v) (vector-set! uf 3 v))
(define (uf-param-names uf) (vector-ref uf 4))
(define (uf-set-param-names! uf v) (vector-set! uf 4 v))
(define (uf-body-forms uf) (vector-ref uf 5))
(define (uf-nbody uf) (vector-ref uf 6))
(define (uf-parent-form uf) (vector-ref uf 7))
(define (uf-body-offset uf) (vector-ref uf 8))

;;; lifted-lambda: #(form func-idx type-idx nfree free-vars noriginal is-closure)
(define (make-lifted-lambda form func-idx type-idx nfree free-vars noriginal is-closure)
  (vector form func-idx type-idx nfree free-vars noriginal is-closure))
(define (ll-form ll) (vector-ref ll 0))
(define (ll-func-idx ll) (vector-ref ll 1))
(define (ll-set-func-idx! ll v) (vector-set! ll 1 v))
(define (ll-type-idx ll) (vector-ref ll 2))
(define (ll-set-type-idx! ll v) (vector-set! ll 2 v))
(define (ll-nfree ll) (vector-ref ll 3))
(define (ll-free-vars ll) (vector-ref ll 4))
(define (ll-noriginal ll) (vector-ref ll 5))
(define (ll-is-closure ll) (vector-ref ll 6))
(define (ll-set-is-closure! ll v) (vector-set! ll 6 v))

;;; global-var: #(name idx)
(define (make-global-var name idx) (vector name idx))
(define (gv-name gv) (vector-ref gv 0))
(define (gv-idx gv) (vector-ref gv 1))

;;; external: #(export-name internal-func-idx nparams param-types return-type wrapper-func-idx wrapper-type-idx)
(define (make-external export-name internal-func-idx nparams param-types return-type)
  (vector export-name internal-func-idx nparams param-types return-type #f #f))
(define (ext-export-name ext) (vector-ref ext 0))
(define (ext-internal-func-idx ext) (vector-ref ext 1))
(define (ext-set-internal-func-idx! ext v) (vector-set! ext 1 v))
(define (ext-nparams ext) (vector-ref ext 2))
(define (ext-param-types ext) (vector-ref ext 3))
(define (ext-return-type ext) (vector-ref ext 4))
(define (ext-wrapper-func-idx ext) (vector-ref ext 5))
(define (ext-set-wrapper-func-idx! ext v) (vector-set! ext 5 v))
(define (ext-wrapper-type-idx ext) (vector-ref ext 6))
(define (ext-set-wrapper-type-idx! ext v) (vector-set! ext 6 v))

;;; analysis-result: #(flags user-funcs globals externals lifted-lambdas arities tracked-forms fn-user-start ty-user-start boxed-vars index-map)
(define (make-analysis-result flags funcs globals externals lambdas arities tracked fn-user-start ty-user-start boxed-vars index-map)
  (vector flags funcs globals externals lambdas arities tracked fn-user-start ty-user-start boxed-vars index-map))
(define (ar-flags ar) (vector-ref ar 0))
(define (ar-funcs ar) (vector-ref ar 1))
(define (ar-globals ar) (vector-ref ar 2))
(define (ar-externals ar) (vector-ref ar 3))
(define (ar-lambdas ar) (vector-ref ar 4))
(define (ar-arities ar) (vector-ref ar 5))
(define (ar-tracked-forms ar) (vector-ref ar 6))
(define (ar-fn-user-start ar) (vector-ref ar 7))
(define (ar-ty-user-start ar) (vector-ref ar 8))
(define (ar-boxed-vars ar) (vector-ref ar 9))
(define (ar-index-map ar) (vector-ref ar 10))

(define (ar-needs-display? ar) (vector-ref (ar-flags ar) FLAG-DISPLAY))
(define (ar-needs-newline? ar) (vector-ref (ar-flags ar) FLAG-NEWLINE))

;;; --- collect-lambdas (ported from lambda_lift.c:81-413) ---

(define (collect-lambdas expr bound gnames
                         funcs-gl lambdas-gl arities)
  ;; funcs-gl: glist of user-func (O(1) append)
  ;; lambdas-gl: glist of lifted-lambda (O(1) append)
  ;; arities: mutable vector with count at index 64
  ;; bound: list of bound variable name strings

  (when (pair? expr)
    (let ((head (car expr)))
      (cond
        ;; (lambda (params...) body...) — not at top-level define
        ((and (symbol? head) (string=? (symbol->string head) "lambda")
              (>= (length expr) 3) (list? (cadr expr)))
         (let* ((params (cadr expr))
                (noriginal (length params))
                (fv (find-free-vars expr '() gnames))
                (nfree (length fv))
                (total (+ nfree noriginal))
                (pnames (append fv (map (lambda (p) (symbol->string p)) params)))
                (tidx (find-or-add-arity! arities total))
                (fi (glist-count funcs-gl))
                (uf (make-user-func #f fi tidx total pnames
                                    (cddr expr) (- (length expr) 2)
                                    expr 2))
                (ll (make-lifted-lambda expr fi tidx
                                        nfree fv noriginal #t)))
           (glist-add! funcs-gl uf)
           (glist-add! lambdas-gl ll)))

        ;; (quote ...) — skip, no lambdas in quoted data
        ((and (symbol? head) (string=? (symbol->string head) "quote"))
         #f)

        ;; (let ((x e) ...) body...) — just recurse, no lifting
        ((and (symbol? head) (string=? (symbol->string head) "let")
              (>= (length expr) 3) (list? (cadr expr)))
         (let ((bnames (map (lambda (b)
                              (if (and (pair? b) (>= (length b) 1) (symbol? (car b)))
                                  (symbol->string (car b)) "_"))
                            (cadr expr))))
           ;; Recurse into init exprs (outer scope)
           (for-each
            (lambda (b)
              (when (and (pair? b) (= (length b) 2))
                (collect-lambdas (cadr b) bound gnames
                                 funcs-gl lambdas-gl arities)))
            (cadr expr))
           ;; Recurse into body (bindings in scope)
           (let ((new-bound (append bnames bound)))
             (for-each
              (lambda (b)
                (collect-lambdas b new-bound gnames
                                 funcs-gl lambdas-gl arities))
              (cddr expr)))))

        ;; (let* ((x e) ...) body...) — just recurse, no lifting
        ((and (symbol? head) (string=? (symbol->string head) "let*")
              (>= (length expr) 3) (list? (cadr expr)))
         (let* ((bindings (cadr expr))
                (bnames (map (lambda (b)
                               (if (and (pair? b) (>= (length b) 1) (symbol? (car b)))
                                   (symbol->string (car b)) "_"))
                             bindings)))
           ;; Recurse into init exprs with progressive scope
           (let loop ((bs bindings) (bn bnames) (cur-bound bound))
             (when (pair? bs)
               (when (and (pair? (car bs)) (= (length (car bs)) 2))
                 (collect-lambdas (cadar bs) cur-bound gnames
                                  funcs-gl lambdas-gl arities))
               (loop (cdr bs) (cdr bn) (cons (car bn) cur-bound))))
           ;; Recurse into body with all bindings
           (for-each
            (lambda (b)
              (collect-lambdas b (append bnames bound) gnames
                               funcs-gl lambdas-gl arities))
            (cddr expr))))

        ;; default: recurse into all sub-expressions
        (else
         (for-each
          (lambda (e)
            (collect-lambdas e bound gnames
                             funcs-gl lambdas-gl arities))
          expr)
         ;; Immediately-applied lambda detection: ((lambda ...) args...)
         (when (and (>= (length expr) 2)
                    (pair? (car expr))
                    (>= (length (car expr)) 3)
                    (symbol? (caar expr))
                    (string=? (symbol->string (caar expr)) "lambda")
                    (list? (cadar expr)))
           (let ((lam-node (car expr)))
             (let loop ((lls (glist-head lambdas-gl)))
               (when (pair? lls)
                 (if (eq? (ll-form (car lls)) lam-node)
                     (ll-set-is-closure! (car lls) #f)
                     (loop (cdr lls))))))))))))

;;; --- WASM type parsing ---

(define (parse-wasm-type name)
  (cond
    ((string=? name "i32") #x7f)  ;; TYPE_I32
    ((string=? name "f64") #x7c)  ;; TYPE_F64
    (else 0)))  ;; void (or unknown) → 0 sentinel

;; Compute boxed-vars without using set!-on-captured-var pattern
(define (compute-boxed-vars lambdas set-targets)
  (let ((st-hash (make-string-ht))
        (boxed-hash (make-string-ht)))
    ;; Build hash set for set-targets
    (let loop ((st set-targets))
      (when (pair? st)
        (string-ht-set! st-hash (car st) #t)
        (loop (cdr st))))
    (let loop-ll ((ls lambdas) (boxed '()))
      (if (null? ls)
          boxed
          (let loop-fv ((fvs (ll-free-vars (car ls))) (boxed boxed))
            (if (null? fvs)
                (loop-ll (cdr ls) boxed)
                (let ((fv (car fvs)))
                  (loop-fv (cdr fvs)
                           (if (and (string-ht-has? st-hash fv) (not (string-ht-has? boxed-hash fv)))
                               (begin
                                 (string-ht-set! boxed-hash fv #t)
                                 (cons fv boxed))
                               boxed)))))))))

;; Build tracked forms set without using set!-on-captured-var pattern
(define (build-tracked funcs lambdas)
  (let loop-f ((fs funcs) (tracked '()))
    (if (null? fs)
        (let loop-l ((ls lambdas) (tracked tracked))
          (if (null? ls)
              tracked
              (let ((ll (car ls)))
                (loop-l (cdr ls)
                        (if (ll-form ll)
                            (cons (cons (ll-form ll) #t) tracked)
                            tracked)))))
        (let ((uf (car fs)))
          (loop-f (cdr fs)
                  (if (uf-parent-form uf)
                      (cons (cons (uf-parent-form uf) #t) tracked)
                      tracked))))))

;;; --- Alpha-conversion: rename local bindings to unique names ---
;;; Prevents boxing analysis name collisions across scopes.

(define *alpha-counter* 0)

(define (alpha-fresh sym)
  (set! *alpha-counter* (+ *alpha-counter* 1))
  (string->symbol
    (string-append (symbol->string sym) "__" (number->string *alpha-counter*))))

(define (alpha-extend-env env binds)
  ;; binds is list of (string . symbol) pairs; return new hash table
  (let ((ht (vector-copy env)))
    (let loop ((bs binds))
      (when (pair? bs)
        (string-ht-set! ht (caar bs) (cdar bs))
        (loop (cdr bs))))
    ht))

(define (alpha-convert form)
  ;; Use hash table for O(1) env lookups instead of O(n) list scans
  (define (walk expr env)
    (cond
      ((symbol? expr)
       (let ((b (string-ht-ref env (symbol->string expr) #f)))
         (if b b expr)))
      ((not (pair? expr)) expr)
      ((not (symbol? (car expr))) (walk-list expr env))
      (else
       (let* ((head-str (symbol->string (car expr)))
              (fc (string-ref head-str 0)))
       (cond
      ((char=? fc #\q)
       (if (string=? head-str "quote") expr
           (walk-list expr env)))
      ((char=? fc #\l)
       (cond
        ((string=? head-str "lambda")
         (if (and (>= (length expr) 3) (list? (cadr expr)))
             (let* ((params (cadr expr))
                    (new-binds (map (lambda (p)
                                     (cons (symbol->string p) (alpha-fresh p)))
                                   params))
                    (new-env (alpha-extend-env env new-binds)))
               (cons 'lambda
                     (cons (map cdr new-binds)
                           (walk-list (cddr expr) new-env))))
             expr))
        ((string=? head-str "let")
         (if (and (>= (length expr) 3) (list? (cadr expr)))
             (let* ((bindings (cadr expr))
                    (new-binds (map (lambda (b)
                                     (cons (symbol->string (car b))
                                           (alpha-fresh (car b))))
                                   bindings))
                    (new-env (alpha-extend-env env new-binds)))
               (cons 'let
                     (cons (map (lambda (b nb)
                                  (list (cdr nb) (walk (cadr b) env)))
                                bindings new-binds)
                           (walk-list (cddr expr) new-env))))
             expr))
        ((string=? head-str "let*")
         (if (and (>= (length expr) 3) (list? (cadr expr)))
             (let* ((bindings (cadr expr))
                    (new-binds (map (lambda (b)
                                     (cons (symbol->string (car b))
                                           (alpha-fresh (car b))))
                                   bindings))
                    ;; Use shared hash table, incrementally extended per binding
                    (ht (vector-copy env)))
               (let loop ((bs bindings) (nbs new-binds) (acc '()))
                 (if (null? bs)
                     (cons 'let*
                           (cons (reverse acc)
                                 (walk-list (cddr expr) ht)))
                     (begin
                       (let ((walked (walk (cadar bs) ht)))
                         (string-ht-set! ht (caar nbs) (cdar nbs))
                         (loop (cdr bs) (cdr nbs)
                               (cons (list (cdar nbs) walked) acc)))))))
             expr))
        (else (walk-list expr env))))
      ((char=? fc #\s)
       (if (string=? head-str "set!")
           (if (and (>= (length expr) 3) (symbol? (cadr expr)))
               (let* ((target (cadr expr))
                      (b (string-ht-ref env (symbol->string target) #f)))
                 (list 'set! (if b b target) (walk (caddr expr) env)))
               expr)
           (walk-list expr env)))
      ((char=? fc #\d)
       (if (string=? head-str "define")
           (cond
             ((and (>= (length expr) 3) (symbol? (cadr expr)))
              (cons 'define (cons (cadr expr) (walk-list (cddr expr) env))))
             ((and (>= (length expr) 3) (pair? (cadr expr)))
              (let* ((sig (cadr expr))
                     (params (cdr sig))
                     (new-binds (map (lambda (p)
                                       (cons (symbol->string p) (alpha-fresh p)))
                                     params))
                     (new-env (alpha-extend-env env new-binds)))
                (cons 'define
                      (cons (cons (car sig) (map cdr new-binds))
                            (walk-list (cddr expr) new-env)))))
             (else expr))
           (walk-list expr env)))
      (else (walk-list expr env)))))))
  (define (walk-list lst env)
    (if (null? lst) '()
        (cons (walk (car lst) env) (walk-list (cdr lst) env))))
  (walk form (make-string-ht)))

(define (alpha-convert-forms forms)
  (set! *alpha-counter* 0)
  (map alpha-convert forms))

;;; --- assign-indices!: assign final WASM function indices ---

(define (assign-indices! funcs lambdas externals fn-user-start ty-user-start)
  ;; Assign func-idx = fn-user-start + position to all user-funcs and
  ;; update corresponding lifted-lambdas and externals to match.
  ;; Also fix up type-idx by adding ty-user-start (arity positions → WASM types).
  (let loop ((fs funcs) (i 0))
    (when (pair? fs)
      (let ((uf (car fs)))
        (uf-set-func-idx! uf (+ fn-user-start i))
        (uf-set-type-idx! uf (+ ty-user-start (uf-type-idx uf))))
      (loop (cdr fs) (+ i 1))))
  ;; Update lifted-lambdas: position → final index, fix type-idx
  (for-each
   (lambda (ll)
     (let ((pos (ll-func-idx ll)))
       (ll-set-func-idx! ll (+ fn-user-start pos))
       (ll-set-type-idx! ll (+ ty-user-start (ll-type-idx ll)))))
   lambdas)
  ;; Update externals: position → final index
  (for-each
   (lambda (ext)
     (let ((pos (ext-internal-func-idx ext)))
       (ext-set-internal-func-idx! ext (+ fn-user-start pos))))
   externals))

;;; --- Reachability-based DCE ---

(define (collect-refs expr known-ht form-to-id)
  ;; Walk an expression, return list of referenced names (user-funcs,
  ;; globals, builtin flags, lambda synthetic IDs).
  ;; known-ht: string-ht mapping user-func/global names → #t
  ;; form-to-id: alist mapping lambda form (by eq?) → synthetic ID string
  (let ((refs '())
        (seen (make-string-ht)))
    (define (add-ref name)
      (unless (string-ht-has? seen name)
        (string-ht-set! seen name #t)
        (set! refs (cons name refs))))
    (define (add-builtin-flag s)
      ;; Map a procedure name to its flag name via *scan-ht* → *flag-names*
      (let ((flag-idx (string-ht-ref *scan-ht* s #f)))
        (when flag-idx
          (add-ref (vector-ref *flag-names* flag-idx)))))
    (define (walk expr)
      (cond
        ((symbol? expr)
         (let ((s (symbol->string expr)))
           (when (string-ht-has? known-ht s)
             (add-ref s))
           (add-builtin-flag s)))
        ((char? expr) (add-ref "char"))
        ((flonum? expr) (add-ref "flonum"))
        ((##ratnum? expr) (add-ref "rational"))
        ((##cpxnum? expr)
         (add-ref "complex")
         (walk (##cpxnum-real expr))
         (walk (##cpxnum-imag expr)))
        ((pair? expr)
         (let ((head (car expr)))
           (cond
             ((and (symbol? head) (string=? (symbol->string head) "quote"))
              ;; Only check for quoted symbols
              (when (and (= (length expr) 2) (symbol? (cadr expr)))
                (add-ref "symbol")))
             ((and (symbol? head) (string=? (symbol->string head) "lambda")
                   (>= (length expr) 3) (list? (cadr expr)))
              ;; Look up this lambda form in form-to-id
              (let loop ((fti form-to-id))
                (when (pair? fti)
                  (if (eq? (caar fti) expr)
                      (add-ref (cdar fti))
                      (loop (cdr fti)))))
              ;; Still recurse into body
              (for-each walk (cddr expr)))
             (else
              ;; Check special cases (display with port, division, etc.)
              (when (symbol? head)
                (let ((s (symbol->string head)))
                  (when (string-ht-has? known-ht s)
                    (add-ref s))
                  (add-builtin-flag s)
                  (cond
                    ((string=? s "display")
                     (add-ref "display"))
                    ((string=? s "write")
                     (add-ref "write"))
                    ((and (string=? s "/") (= (length expr) 3))
                     (add-ref "rational"))
                    ((string=? s "write-bytevector")
                     (add-ref "bytevector")
                     (add-ref "file-io"))
                    ((string=? s "define-external")
                     (when (>= (length expr) 3)
                       (let ((sig (cadr expr)))
                         (when (pair? sig)
                           (for-each
                            (lambda (param)
                              (when (and (pair? param)
                                         (>= (length param) 1)
                                         (symbol? (car param))
                                         (string=? (symbol->string (car param)) "f64"))
                                (add-ref "flonum")))
                            (cdr sig))))
                       (when (and (>= (length expr) 3)
                                  (symbol? (caddr expr))
                                  (string=? (symbol->string (caddr expr)) "f64"))
                         (add-ref "flonum"))))
                    (else #f))))
              ;; Recurse into sub-expressions
              ;; If head is symbol, already handled above — recurse into args only
              ;; If head is not symbol (e.g., let bindings), recurse into everything
              (if (symbol? head)
                  (for-each walk (cdr expr))
                  (for-each walk expr))))))))
    (walk expr)
    refs))

(define (build-dep-graph funcs globals externals lambdas forms known-ht)
  ;; Build form→id map for anonymous lambdas
  (let ((form-to-id '())
        (lambda-counter 0)
        (deps-ht (make-string-ht))
        (roots '())
        (roots-seen (make-string-ht)))

    ;; Assign synthetic IDs to anonymous lambdas
    (for-each
     (lambda (ll)
       (let ((id (string-append "#lambda:" (number->string lambda-counter))))
         (set! form-to-id (cons (cons (ll-form ll) id) form-to-id))
         (set! lambda-counter (+ lambda-counter 1))))
     lambdas)

    ;; Build deps for named user-funcs
    (for-each
     (lambda (uf)
       (when (uf-name uf)
         (let ((refs (if (uf-body-forms uf)
                         (collect-refs-body (uf-body-forms uf) known-ht form-to-id)
                         '())))
           (string-ht-set! deps-ht (uf-name uf) refs))))
     funcs)

    ;; Build deps for anonymous lambdas (via form-to-id)
    (for-each
     (lambda (ll)
       (let ((id (let loop ((fti form-to-id))
                   (cond ((null? fti) #f)
                         ((eq? (caar fti) (ll-form ll)) (cdar fti))
                         (else (loop (cdr fti)))))))
         (when id
           (let* ((lam (ll-form ll))
                  (refs (collect-refs-body (cddr lam) known-ht form-to-id)))
             (string-ht-set! deps-ht id refs)))))
     lambdas)

    ;; Collect roots: top-level non-define exprs, var-define init exprs, externals
    (for-each
     (lambda (form)
       (cond
         ((and (not (is-func-define? form))
               (not (is-var-define? form))
               (not (is-external-define? form)))
          (for-each (lambda (name)
                      (unless (string-ht-has? roots-seen name)
                        (string-ht-set! roots-seen name #t)
                        (set! roots (cons name roots))))
                    (collect-refs form known-ht form-to-id)))
         ((is-var-define? form)
          (for-each (lambda (name)
                      (unless (string-ht-has? roots-seen name)
                        (string-ht-set! roots-seen name #t)
                        (set! roots (cons name roots))))
                    (collect-refs (caddr form) known-ht form-to-id)))))
     forms)

    ;; External functions are always roots
    (for-each
     (lambda (ext)
       (let ((name (ext-export-name ext)))
         (unless (string-ht-has? roots-seen name)
           (string-ht-set! roots-seen name #t)
           (set! roots (cons name roots)))))
     externals)

    ;; WIT export functions are always roots
    (when *wit-world*
      (for-each
       (lambda (item)
         (when (and (pair? item) (eq? (car item) 'export)
                    (= (length item) 3))
           (let ((name (cadr item)))
             (unless (string-ht-has? roots-seen name)
               (string-ht-set! roots-seen name #t)
               (set! roots (cons name roots))))))
       (wit-world-items *wit-world*))
      ;; WIT import wrapper functions are always roots
      (for-each
       (lambda (item)
         (when (and (pair? item) (eq? (car item) 'import)
                    (= (length item) 3))
           (let ((name (cadr item)))
             (unless (string-ht-has? roots-seen name)
               (string-ht-set! roots-seen name #t)
               (set! roots (cons name roots))))))
       (wit-world-items *wit-world*)))

    (vector roots deps-ht form-to-id)))

(define (collect-refs-body body-forms known-ht form-to-id)
  ;; Collect refs from multiple body forms
  (let ((all-refs '())
        (seen (make-string-ht)))
    (for-each
     (lambda (form)
       (for-each
        (lambda (ref)
          (unless (string-ht-has? seen ref)
            (string-ht-set! seen ref #t)
            (set! all-refs (cons ref all-refs))))
        (collect-refs form known-ht form-to-id)))
     body-forms)
    all-refs))

(define (mark-reachable roots deps-ht)
  ;; Standard worklist reachability. Returns string-ht of reachable names.
  (let ((reachable (make-string-ht))
        (worklist roots))
    (let loop ()
      (when (pair? worklist)
        (let ((name (car worklist)))
          (set! worklist (cdr worklist))
          (unless (string-ht-has? reachable name)
            (string-ht-set! reachable name #t)
            ;; Push user/lambda deps
            (let ((deps (string-ht-ref deps-ht name #f)))
              (when deps
                (for-each
                 (lambda (d)
                   (unless (string-ht-has? reachable d)
                     (set! worklist (cons d worklist))))
                 deps)))
            ;; Push builtin deps from *builtin-deps* table
            (let loop-bd ((bd *builtin-deps*))
              (when (pair? bd)
                (when (string=? (caar bd) name)
                  (for-each
                   (lambda (d)
                     (unless (string-ht-has? reachable d)
                       (set! worklist (cons d worklist))))
                   (cdar bd)))
                (loop-bd (cdr bd))))))
        (loop)))
    reachable))

(define (reachable-to-flags reachable)
  ;; Convert reachable set (string-ht) to flags vector.
  ;; Maps flag names back to FLAG indices.
  ;; All dependency propagation is handled by mark-reachable via *builtin-deps*.
  (let ((flags (make-flags)))
    (let loop ((i 0))
      (when (< i FLAG-COUNT)
        (when (string-ht-has? reachable (vector-ref *flag-names* i))
          (vector-set! flags i #t))
        (loop (+ i 1))))
    flags))

(define (compute-index-map flags target)
  ;; Given flags vector and target, compute ALL function/type indices.
  ;; Returns a vector of size IDX-MAP-SIZE.
  (let ((needs-display (vector-ref flags FLAG-DISPLAY))
        (needs-newline (vector-ref flags FLAG-NEWLINE))
        (needs-eqv (vector-ref flags FLAG-EQV))
        (needs-equal (vector-ref flags FLAG-EQUAL))
        (needs-num-to-str (vector-ref flags FLAG-NUM-TO-STR))
        (needs-str-to-num (vector-ref flags FLAG-STR-TO-NUM))
        (needs-flonum (vector-ref flags FLAG-FLONUM))
        (needs-rational (vector-ref flags FLAG-RATIONAL))
        (needs-complex (vector-ref flags FLAG-COMPLEX))
        (needs-math (vector-ref flags FLAG-MATH))
        (needs-command-line (vector-ref flags FLAG-COMMAND-LINE))
        (needs-string-append (vector-ref flags FLAG-STRING-APPEND))
        (needs-bytevector (vector-ref flags FLAG-BYTEVECTOR))
        (needs-char (vector-ref flags FLAG-CHAR))
        (needs-symbol (vector-ref flags FLAG-SYMBOL))
        (needs-file-io (vector-ref flags FLAG-FILE-IO))
        (needs-string-ops (vector-ref flags FLAG-STRING-OPS))
        (needs-read (vector-ref flags FLAG-READ))
        (needs-vector (vector-ref flags FLAG-VECTOR))
        (needs-write (vector-ref flags FLAG-WRITE))
        (needs-clock (vector-ref flags FLAG-CLOCK))
        (needs-apply (vector-ref flags FLAG-APPLY))
        (needs-file-exists (vector-ref flags FLAG-FILE-EXISTS))
        (needs-bv-copy (vector-ref flags FLAG-BV-COPY))
        (needs-bv-append (vector-ref flags FLAG-BV-APPEND))
        (needs-utf8-string (vector-ref flags FLAG-UTF8-STRING))
        (needs-get-env (vector-ref flags FLAG-GET-ENV))
        (needs-exit (vector-ref flags FLAG-EXIT))
        (needs-promise (vector-ref flags FLAG-PROMISE))
        (nimports (if *wit-world*
                      (length (wit-world-func-imports *wit-world*))
                      0))
        (nbuiltins 1)
        (m (make-vector IDX-MAP-SIZE -1)))

    ;; Derived booleans
    (let* ((needs-dispatch (or needs-flonum needs-rational needs-complex))
           (needs-eqv-type (or needs-eqv needs-equal needs-dispatch needs-complex
                               needs-math needs-string-append needs-bytevector
                               needs-file-io needs-string-ops needs-display
                               needs-write needs-apply needs-vector))
           (needs-io (or needs-display needs-newline needs-write needs-file-io))
           (needs-memory (or needs-io needs-bytevector needs-clock needs-get-env needs-command-line))
           (needs-numstr (or needs-num-to-str needs-str-to-num needs-flonum
                             needs-dispatch needs-bytevector needs-display needs-write
                             needs-file-io needs-string-ops needs-vector needs-symbol)))
      (vector-set! m IDX-NEEDS-DISPATCH needs-dispatch)
      (vector-set! m IDX-NEEDS-EQV-TYPE needs-eqv-type)
      (vector-set! m IDX-NEEDS-IO needs-io)
      (vector-set! m IDX-NEEDS-MEMORY needs-memory)
      (vector-set! m IDX-NEEDS-NUMSTR needs-numstr)

      (vector-set! m IDX-NEEDS-EXIT needs-exit)

      ;; Import function indices
      (when (not *wit-world*)
        (when needs-exit
          (vector-set! m IDX-FN-PROC-EXIT nimports) (set! nimports (+ nimports 1)))
        (when needs-io
          (vector-set! m IDX-FN-GET-STDOUT nimports) (set! nimports (+ nimports 1))
          (vector-set! m IDX-FN-GET-STDERR nimports) (set! nimports (+ nimports 1))
          (vector-set! m IDX-FN-STREAM-WRITE nimports) (set! nimports (+ nimports 1)))
        (when needs-clock
          (vector-set! m IDX-FN-CLOCK-NOW nimports) (set! nimports (+ nimports 1)))
        (when needs-command-line
          (vector-set! m IDX-FN-GET-ARGUMENTS nimports) (set! nimports (+ nimports 1)))
        (when needs-get-env
          (vector-set! m IDX-FN-GET-ENVIRONMENT nimports) (set! nimports (+ nimports 1)))
        (when needs-file-io
          (vector-set! m IDX-FN-GET-STDIN nimports) (set! nimports (+ nimports 1))
          (vector-set! m IDX-FN-STREAM-READ nimports) (set! nimports (+ nimports 1))
          (vector-set! m IDX-FN-GET-DIRECTORIES nimports) (set! nimports (+ nimports 1))
          (vector-set! m IDX-FN-OPEN-AT nimports) (set! nimports (+ nimports 1))
          (vector-set! m IDX-FN-READ-VIA-STREAM nimports) (set! nimports (+ nimports 1))
          (vector-set! m IDX-FN-WRITE-VIA-STREAM nimports) (set! nimports (+ nimports 1))
          (vector-set! m IDX-FN-DROP-DESCRIPTOR nimports) (set! nimports (+ nimports 1))
          (vector-set! m IDX-FN-DROP-INPUT-STREAM nimports) (set! nimports (+ nimports 1))
          (vector-set! m IDX-FN-DROP-OUTPUT-STREAM nimports) (set! nimports (+ nimports 1))))

      ;; Builtin function indices
      (when needs-display
        (vector-set! m IDX-FN-DISPLAY (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1)))
      (when needs-write
        (vector-set! m IDX-FN-WRITE (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1)))
      (when needs-newline
        (vector-set! m IDX-FN-NEWLINE (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1)))
      (when needs-eqv
        (vector-set! m IDX-FN-EQV (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1)))
      (when needs-equal
        (vector-set! m IDX-FN-EQUAL (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1)))
      (when needs-num-to-str
        (vector-set! m IDX-FN-NUM-TO-STR (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1)))
      (when needs-str-to-num
        (vector-set! m IDX-FN-STR-TO-NUM (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1)))
      (when needs-dispatch
        (vector-set! m IDX-FN-FLONUM-START (+ nimports nbuiltins))
        (set! nbuiltins (+ nbuiltins FL_COUNT)))
      (when needs-rational
        (vector-set! m IDX-FN-GCD (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1)))
      (when needs-complex
        (vector-set! m IDX-FN-MAKE-COMPLEX (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1)))
      (when needs-math
        (vector-set! m IDX-FN-MATH-START (+ nimports nbuiltins))
        (set! nbuiltins (+ nbuiltins MATH_COUNT)))
      (when needs-command-line
        (vector-set! m IDX-FN-COMMAND-LINE (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1)))
      (when needs-string-append
        (vector-set! m IDX-FN-STRING-APPEND (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1)))
      (when needs-bytevector
        (vector-set! m IDX-FN-LINEAR-ALLOC (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-BV-ALLOC (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-BV-COPY-STR (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-PTR-TO-STR (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-BV-ALLOC-FILL (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1)))
      (when needs-bv-copy
        (vector-set! m IDX-FN-BV-COPY-RANGE (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-BV-COPY (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-BV-COPY-FROM (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1)))
      (when needs-bv-append
        (vector-set! m IDX-FN-BV-APPEND (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1)))
      (when needs-utf8-string
        (vector-set! m IDX-FN-UTF8-TO-STR-RANGE (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-UTF8-TO-STR (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-UTF8-TO-STR-FROM (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-STR-TO-UTF8-RANGE (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-STR-TO-UTF8 (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-STR-TO-UTF8-FROM (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1)))
      (when needs-file-io
        (vector-set! m IDX-FN-OPEN-INPUT-FILE (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-OPEN-OUTPUT-FILE (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-CLOSE-PORT (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-READ-CHAR (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-PEEK-CHAR (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-WRITE-CHAR (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-OPEN-INPUT-STRING (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1)))
      (when needs-string-ops
        (vector-set! m IDX-FN-SUBSTRING (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-STRING-COPY (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-STRING-TO-LIST (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-LIST-TO-STRING (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-STRING-EQ (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-STRING-LT (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-STRING-CI-EQ (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-STRING-CI-LT (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-STRING-FILL (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1)))
      (when needs-read
        (vector-set! m IDX-FN-READ (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-READ-LIST (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-READ-STRING (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1)))
      (when needs-vector
        (vector-set! m IDX-FN-VECTOR-COPY (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-LIST-TO-VECTOR (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-VECTOR-TO-LIST (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-VECTOR-FILL (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1)))
      (when needs-symbol
        (vector-set! m IDX-FN-INTERN-SYM (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1)))
      (when needs-apply
        (vector-set! m IDX-FN-APPLY (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1)))
      (when needs-file-exists
        (vector-set! m IDX-FN-FILE-EXISTS (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1)))
      (when needs-get-env
        (vector-set! m IDX-FN-GET-ENV-VAR (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1))
        (vector-set! m IDX-FN-GET-ENV-VARS (+ nimports nbuiltins)) (set! nbuiltins (+ nbuiltins 1)))
      (vector-set! m IDX-FN-USER-START (+ nimports nbuiltins))

      ;; Type indices
      (let ((offset TY_FIXED_COUNT))
        (when needs-char (vector-set! m IDX-TY-CHAR offset) (set! offset (+ offset 1)))
        (when needs-eqv-type (vector-set! m IDX-TY-EQV offset) (set! offset (+ offset 1)))
        (when needs-symbol (vector-set! m IDX-TY-SYMBOL offset) (set! offset (+ offset 1)))
        (when needs-numstr (vector-set! m IDX-TY-NUMSTR offset) (set! offset (+ offset 1)))
        (when needs-flonum (vector-set! m IDX-TY-FLONUM offset) (set! offset (+ offset 1)))
        (when needs-rational (vector-set! m IDX-TY-RATIONAL offset) (set! offset (+ offset 1)))
        (when needs-complex (vector-set! m IDX-TY-COMPLEX offset) (set! offset (+ offset 1)))
        (when (or needs-command-line needs-get-env) (vector-set! m IDX-TY-ARGS-IMPORT offset) (set! offset (+ offset 1)))
        (when (or needs-newline needs-command-line needs-get-env)
          (vector-set! m IDX-TY-VOID-EQREF offset) (set! offset (+ offset 1)))
        (when needs-bytevector
          (vector-set! m IDX-TY-BYTEVECTOR offset) (set! offset (+ offset 1))
          (vector-set! m IDX-TY-BV-COPY-STR offset) (set! offset (+ offset 1)))
        (when needs-file-io
          (vector-set! m IDX-TY-PORT offset) (set! offset (+ offset 1))
          (vector-set! m IDX-TY-EOF offset) (set! offset (+ offset 1))
          (vector-set! m IDX-TY-STREAM-READ offset) (set! offset (+ offset 1))
          (vector-set! m IDX-TY-OPEN-AT offset) (set! offset (+ offset 1)))
        (when needs-string-ops
          (vector-set! m IDX-TY-STR-3ARG offset) (set! offset (+ offset 1)))
        (when needs-clock
          (vector-set! m IDX-TY-CLOCK-IMPORT offset) (set! offset (+ offset 1)))
        (when (not *wit-world*)
          (vector-set! m IDX-TY-VOID-I32 offset) (set! offset (+ offset 1)))
        (when needs-clock
          (vector-set! m IDX-TY-VOID-I64 offset) (set! offset (+ offset 1)))
        (when needs-promise (vector-set! m IDX-TY-PROMISE offset) (set! offset (+ offset 1)))
        (vector-set! m IDX-TY-VECTOR TY-ENV)
        (vector-set! m IDX-TY-USER-START offset))

      ;; Store counts
      (vector-set! m IDX-NIMPORTS nimports)
      (vector-set! m IDX-NBUILTINS nbuiltins)
      (vector-set! m IDX-WIT-NIMPORTS
                   (if *wit-world* (length (wit-world-func-imports *wit-world*)) 0))
      m)))

;;; --- analyze-forms: main orchestrator ---

(define (analyze-forms forms target)
  ;; 1. Collect named functions, globals, externals
  (let ((funcs '())
        (globals '())
        (externals '())
        (arities (make-vector 65 0))
        (analyze-t0 (if *profile* (current-milliseconds) 0)))
    (vector-set! arities 64 0)

        (for-each
         (lambda (form)
           (cond
             ((is-external-define? form)
              (let* ((sig (cadr form))
                     (ename (symbol->string (car sig)))
                     (np (- (length sig) 1))
                     (pnames (if (> np 0)
                                 (map (lambda (tp) (symbol->string (cadr tp))) (cdr sig))
                                 '()))
                     (ptypes (if (> np 0)
                                 (map (lambda (tp) (parse-wasm-type (symbol->string (car tp)))) (cdr sig))
                                 '()))
                     (rtype (parse-wasm-type (symbol->string (caddr form))))
                     (tidx (find-or-add-arity! arities np))
                     (fi (length funcs))
                     (uf (make-user-func ename fi tidx np pnames
                                         (cdddr form) (- (length form) 3)
                                         form 3))
                     (ext (make-external ename fi np ptypes rtype)))
                (set! funcs (append funcs (list uf)))
                (set! externals (append externals (list ext)))))

             ((is-func-define-sugar? form)
              (let* ((sig (cadr form))
                     (np (- (length sig) 1))
                     (pnames (if (> np 0)
                                 (map (lambda (p) (symbol->string p)) (cdr sig))
                                 '()))
                     (tidx (find-or-add-arity! arities np))
                     (fi (length funcs))
                     (uf (make-user-func (symbol->string (car sig)) fi tidx np pnames
                                         (cddr form) (- (length form) 2)
                                         form 2)))
                (set! funcs (append funcs (list uf)))))

             ((is-func-define-lambda? form)
              (let* ((lam (caddr form))
                     (params (cadr lam))
                     (np (length params))
                     (pnames (if (> np 0)
                                 (map (lambda (p) (symbol->string p)) params)
                                 '()))
                     (tidx (find-or-add-arity! arities np))
                     (fi (length funcs))
                     (uf (make-user-func (symbol->string (cadr form)) fi tidx np pnames
                                         (cddr lam) (- (length lam) 2)
                                         lam 2)))
                (set! funcs (append funcs (list uf)))))

             ((is-var-define? form)
              (let ((gv (make-global-var (symbol->string (cadr form)) (length globals))))
                (set! globals (append globals (list gv)))))))
         forms)

        (when *profile* (phase-time "  analyze/collect-funcs" analyze-t0))

        ;; 4. Lambda lifting
        (let* ((analyze-t0 (if *profile* (current-milliseconds) 0))
               (gnames (append (map gv-name globals)
                               (let loop ((fs funcs) (acc '()))
                                 (if (null? fs)
                                     acc
                                     (let ((n (uf-name (car fs))))
                                       (loop (cdr fs) (if n (cons n acc) acc)))))))
               (funcs-gl (make-glist))
               (lambdas-gl (make-glist)))

          ;; Seed funcs-gl with existing funcs from collect-funcs phase
          (for-each (lambda (uf) (glist-add! funcs-gl uf)) funcs)

          ;; Collect from top-level expressions and var-define init exprs
          (for-each
           (lambda (form)
             (cond
               ((and (not (is-func-define? form))
                     (not (is-var-define? form))
                     (not (is-external-define? form)))
                (collect-lambdas form '() gnames
                                 funcs-gl lambdas-gl arities))
               ((is-var-define? form)
                (collect-lambdas (caddr form) '() gnames
                                 funcs-gl lambdas-gl arities))))
           forms)

          ;; Collect from all function bodies (including newly added)
          ;; Use cursor-based traversal: glist grows as we process
          (let loop ((cursor (glist-head funcs-gl)))
            (when (pair? cursor)
              (let ((uf (car cursor)))
                (when (uf-body-forms uf)
                  (for-each
                   (lambda (body-form)
                     (collect-lambdas body-form (uf-param-names uf) gnames
                                      funcs-gl lambdas-gl arities))
                   (uf-body-forms uf))))
              (loop (cdr cursor))))

          (when *profile* (phase-time "  analyze/lambda-lift" analyze-t0))

          (let ((funcs (glist-head funcs-gl))
                (lambdas (glist-head lambdas-gl))
                (analyze-t0 (if *profile* (current-milliseconds) 0)))

            ;; 4a-wit. Add synthetic user-funcs for WIT import wrappers
            (when *wit-world*
              (for-each
               (lambda (wf)
                 (let* ((name (wit-func-name wf))
                        (np (length (wit-func-params wf)))
                        (tidx (find-or-add-arity! arities np))
                        (fi (length funcs))
                        (uf (make-user-func name fi tidx np '() #f 0 #f 0)))
                   (set! funcs (append funcs (list uf)))))
               (wit-world-func-imports *wit-world*)))

            ;; 4b. Reachability analysis (build graph, mark-sweep)
            (let* ((known-ht (make-string-ht))
                   (_ (begin
                        ;; Populate known-ht with user func/global names
                        (for-each (lambda (uf) (when (uf-name uf)
                                                 (string-ht-set! known-ht (uf-name uf) #t)))
                                  funcs)
                        (for-each (lambda (gv) (string-ht-set! known-ht (gv-name gv) #t))
                                  globals)))
                   (graph (build-dep-graph funcs globals externals lambdas forms known-ht))
                   (roots (vector-ref graph 0))
                   (deps-ht (vector-ref graph 1))
                   (reachable (mark-reachable roots deps-ht)))

              (when *profile* (phase-time "  analyze/reachability" analyze-t0))

              ;; 5. Compute flags and indices from reachable set
              (let* ((analyze-t0 (if *profile* (current-milliseconds) 0))
                     (flags (reachable-to-flags reachable))
                     (index-map (compute-index-map flags target))
                     (fn-user-start (vector-ref index-map IDX-FN-USER-START))
                     (ty-user-start (vector-ref index-map IDX-TY-USER-START))
                     (form-to-id (vector-ref graph 2)))

                ;; 6. Filter unreachable functions and lambdas
                (let* ((live-funcs
                        (let loop ((fs funcs) (acc '()))
                          (if (null? fs)
                              (reverse acc)
                              (let ((uf (car fs)))
                                (loop (cdr fs)
                                      (cond
                                        ;; Named: keep if reachable or WIT export/import
                                        ((uf-name uf)
                                         (if (or (string-ht-has? reachable (uf-name uf))
                                                 (wit-export-or-import-name? (uf-name uf)))
                                             (cons uf acc)
                                             acc))
                                        ;; Anonymous lambda: look up synthetic ID
                                        (else
                                         (let ((id (let id-loop ((fti form-to-id))
                                                     (cond ((null? fti) #f)
                                                           ((eq? (caar fti) (uf-parent-form uf))
                                                            (cdar fti))
                                                           (else (id-loop (cdr fti)))))))
                                           (if (and id (string-ht-has? reachable id))
                                               (cons uf acc)
                                               acc)))))))))
                       (live-lambdas
                        (let loop ((ls lambdas) (acc '()))
                          (if (null? ls)
                              (reverse acc)
                              (let* ((ll (car ls))
                                     (id (let id-loop ((fti form-to-id))
                                           (cond ((null? fti) #f)
                                                 ((eq? (caar fti) (ll-form ll))
                                                  (cdar fti))
                                                 (else (id-loop (cdr fti)))))))
                                (loop (cdr ls)
                                      (if (and id (string-ht-has? reachable id))
                                          (cons ll acc)
                                          acc))))))
                       ;; Re-number surviving funcs (0, 1, 2, ...)
                       (_ (let loop ((fs live-funcs) (i 0))
                            (when (pair? fs)
                              (uf-set-func-idx! (car fs) i)
                              (loop (cdr fs) (+ i 1)))))
                       ;; Update lambda func-idx to match new positions
                       (_ (for-each
                           (lambda (ll)
                             (let loop ((fs live-funcs) (i 0))
                               (when (pair? fs)
                                 (if (eq? (uf-parent-form (car fs)) (ll-form ll))
                                     (ll-set-func-idx! ll i)
                                     (loop (cdr fs) (+ i 1))))))
                           live-lambdas)))

                  (when *profile* (phase-time "  analyze/filter+indices" analyze-t0))

                  ;; 7. Build tracked forms set
                  (let* ((analyze-t0 (if *profile* (current-milliseconds) 0))
                         (tracked (build-tracked live-funcs live-lambdas)))

                    (when *profile* (phase-time "  analyze/build-tracked" analyze-t0))

                    ;; 8. Compute boxed-vars
                    (let* ((analyze-t0 (if *profile* (current-milliseconds) 0))
                           (set-targets (find-set-targets-all forms))
                           (boxed (compute-boxed-vars live-lambdas set-targets)))

                      (when *profile* (phase-time "  analyze/set+boxed" analyze-t0))

                      ;; 9. Assign final WASM function indices
                      (assign-indices! live-funcs live-lambdas externals fn-user-start ty-user-start)

                      (make-analysis-result flags live-funcs globals externals
                                           live-lambdas arities tracked
                                           fn-user-start ty-user-start boxed index-map))))))))))
