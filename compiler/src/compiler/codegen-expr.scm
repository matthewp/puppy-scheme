;;; codegen-expr.scm — Expression compilation to WASM instructions
;;; Direct translation of codegen_expr.c, operating on native Scheme data.

;;; --- FL_*/MATH_* constants (matching codegen_internal.h) ---

(define FL-ADD 0) (define FL-SUB 1) (define FL-MUL 2) (define FL-DIV 3)
(define FL-NUM-EQ 4) (define FL-NUM-LT 5) (define FL-NUM-GT 6)
(define FL-NUM-LE 7) (define FL-NUM-GE 8)
(define FL-EXACT-TO-INEXACT 9) (define FL-INEXACT-TO-EXACT 10)
(define FL-FLOOR 11) (define FL-CEILING 12) (define FL-TRUNCATE 13) (define FL-ROUND 14)

(define MATH-SQRT 0) (define MATH-EXP 1) (define MATH-LOG 2)
(define MATH-SIN 3) (define MATH-COS 4) (define MATH-TAN 5)
(define MATH-ASIN 6) (define MATH-ACOS 7) (define MATH-ATAN 8)
(define MATH-ATAN2 9) (define MATH-EXPT 10)
(define MATH-UNARY-COUNT 9)

(define (error msg)
  (display msg (current-error-port))
  (newline (current-error-port))
  (exit 1))

;;; --- Boxing/unboxing helpers ---

(define (emit-box-i31! b)
  (wbuf-byte! b OP-GC-PREFIX)
  (wbuf-byte! b GC-REF-I31))

(define (emit-unbox-i31! b)
  (wbuf-byte! b OP-GC-PREFIX)
  (wbuf-byte! b GC-REF-CAST)
  (wbuf-byte! b HT-I31)
  (wbuf-byte! b OP-GC-PREFIX)
  (wbuf-byte! b GC-I31-GET-S))

(define (emit-box-fixnum! b)
  ;; i32 on stack → shl 1 → ref.i31 (tag-bit encoding: fixnums are even)
  (wbuf-byte! b OP-I32-CONST) (wbuf-i32! b 1)
  (wbuf-byte! b OP-I32-SHL)
  (emit-box-i31! b))

(define (emit-unbox-fixnum! b)
  ;; eqref → ref.cast i31 → i31.get_s → shr_s 1 → i32
  (wbuf-byte! b OP-GC-PREFIX)
  (wbuf-byte! b GC-REF-CAST)
  (wbuf-byte! b HT-I31)
  (wbuf-byte! b OP-GC-PREFIX)
  (wbuf-byte! b GC-I31-GET-S)
  (wbuf-byte! b OP-I32-CONST) (wbuf-i32! b 1)
  (wbuf-byte! b OP-I32-SHR-S))

(define (emit-box-bool! b)
  ;; i32 0/1 → (x << 1) | 1 → ref.i31 (maps 0→1=#f, 1→3=#t)
  (wbuf-byte! b OP-I32-CONST) (wbuf-i32! b 1)
  (wbuf-byte! b OP-I32-SHL)
  (wbuf-byte! b OP-I32-CONST) (wbuf-i32! b 1)
  (wbuf-byte! b OP-I32-OR)
  (emit-box-i31! b))

(define (emit-truthy-test! b)
  ;; Safe truthiness test for any eqref value on the stack.
  ;; Produces i32: 0 if #f (i31 1), non-zero otherwise.
  ;; Handles strings, pairs, etc. without crashing.
  (wbuf-byte! b OP-I32-CONST)
  (wbuf-i32! b 1)
  (emit-box-i31! b)            ;; push #f = (i31 1)
  (wbuf-byte! b OP-REF-EQ)    ;; compare: 1 if value is #f, 0 otherwise
  (wbuf-byte! b OP-I32-EQZ))  ;; negate: 0 if #f, 1 otherwise

(define (emit-box-f64! b ty)
  (wbuf-byte! b OP-GC-PREFIX)
  (wbuf-byte! b GC-STRUCT-NEW)
  (wbuf-u32! b ty))

(define (emit-unbox-f64! b ty)
  (wbuf-byte! b OP-GC-PREFIX)
  (wbuf-byte! b GC-REF-CAST)
  (wbuf-u32! b ty)
  (wbuf-byte! b OP-GC-PREFIX)
  (wbuf-byte! b GC-STRUCT-GET)
  (wbuf-u32! b ty)
  (wbuf-u32! b 0))

(define (emit-box-rational! b ty)
  (wbuf-byte! b OP-GC-PREFIX)
  (wbuf-byte! b GC-STRUCT-NEW)
  (wbuf-u32! b ty))

(define (emit-box-complex! b ty)
  (wbuf-byte! b OP-GC-PREFIX)
  (wbuf-byte! b GC-STRUCT-NEW)
  (wbuf-u32! b ty))

;;; --- Intrinsic argument helpers ---

(define (codegen-i32-arg val body ctx)
  ;; Compile val to leave an i32 on the stack (NOT eqref)
  (if (and (integer? val) (exact? val))
      (begin (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body val) #t)
      (and (codegen-expr val body ctx)
           (begin (emit-unbox-fixnum! body) #t))))

(define (codegen-f64-arg val body ctx)
  ;; Compile val to leave an f64 on the stack
  (if (flonum? val)
      (begin (wbuf-byte! body OP-F64-CONST) (wbuf-f64! body val) #t)
      (and (codegen-expr val body ctx)
           (begin (emit-unbox-f64! body (ctx-ty-flonum ctx)) #t))))

(define (emit-void! body)
  (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 7)
  (emit-box-i31! body))

;;; --- Ctx accessors ---
;;; ctx is a vector: #(locals nlocals globals funcs lambdas wrappers
;;;   closure-arities nclosure-arity narity
;;;   fn-display fn-newline fn-eqv fn-equal fn-num-to-str fn-str-to-num
;;;   fn-flonum-start fn-gcd fn-user-start ty-user-start
;;;   ty-flonum ty-rational ty-complex fn-make-complex fn-math-start
;;;   fn-command-line fn-string-append fn-linear-alloc fn-bv-alloc
;;;   fn-bv-copy-str ty-bytevector bump-ptr-idx fn-ptr-to-str
;;;   ty-char ty-eqv ty-symbol needs-rational needs-complex nfuncs
;;;   ...
;;;   74:globals-hash 75:funcs-hash 76:locals-hash 77:boxed-vars-hash)

(define (ctx-locals ctx) (vector-ref ctx 0))
(define (ctx-nlocals ctx) (vector-ref ctx 1))
(define (ctx-globals ctx) (vector-ref ctx 2))
(define (ctx-funcs ctx) (vector-ref ctx 3))
(define (ctx-lambdas ctx) (vector-ref ctx 4))
(define (ctx-wrappers ctx) (vector-ref ctx 5))
(define (ctx-closure-arities ctx) (vector-ref ctx 6))
(define (ctx-nclosure-arity ctx) (vector-ref ctx 7))
(define (ctx-narity ctx) (vector-ref ctx 8))
(define (ctx-fn-display ctx) (vector-ref ctx 9))
(define (ctx-fn-newline ctx) (vector-ref ctx 10))
(define (ctx-fn-eqv ctx) (vector-ref ctx 11))
(define (ctx-fn-equal ctx) (vector-ref ctx 12))
(define (ctx-fn-num-to-str ctx) (vector-ref ctx 13))
(define (ctx-fn-str-to-num ctx) (vector-ref ctx 14))
(define (ctx-fn-flonum-start ctx) (vector-ref ctx 15))
(define (ctx-fn-gcd ctx) (vector-ref ctx 16))
(define (ctx-fn-user-start ctx) (vector-ref ctx 17))
(define (ctx-ty-user-start ctx) (vector-ref ctx 18))
(define (ctx-ty-flonum ctx) (vector-ref ctx 19))
(define (ctx-ty-rational ctx) (vector-ref ctx 20))
(define (ctx-ty-complex ctx) (vector-ref ctx 21))
(define (ctx-fn-make-complex ctx) (vector-ref ctx 22))
(define (ctx-fn-math-start ctx) (vector-ref ctx 23))
(define (ctx-fn-command-line ctx) (vector-ref ctx 24))
(define (ctx-fn-string-append ctx) (vector-ref ctx 25))
(define (ctx-fn-linear-alloc ctx) (vector-ref ctx 26))
(define (ctx-fn-bv-alloc ctx) (vector-ref ctx 27))
(define (ctx-fn-bv-copy-str ctx) (vector-ref ctx 28))
(define (ctx-ty-bytevector ctx) (vector-ref ctx 29))
(define (ctx-bump-ptr-idx ctx) (vector-ref ctx 30))
(define (ctx-fn-ptr-to-str ctx) (vector-ref ctx 31))
(define (ctx-ty-char ctx) (vector-ref ctx 32))
(define (ctx-ty-eqv ctx) (vector-ref ctx 33))
(define (ctx-ty-symbol ctx) (vector-ref ctx 34))
(define (ctx-needs-rational ctx) (vector-ref ctx 35))
(define (ctx-needs-complex ctx) (vector-ref ctx 36))
(define (ctx-nfuncs ctx) (vector-ref ctx 37))
(define (ctx-target ctx) (vector-ref ctx 38))
(define (ctx-fn-io ctx) (vector-ref ctx 39))
(define (ctx-fn-args-sizes-get ctx) (vector-ref ctx 40))
(define (ctx-fn-args-get ctx) (vector-ref ctx 41))
(define (ctx-fn-fd-read ctx) (vector-ref ctx 42))
(define (ctx-fn-fd-close ctx) (vector-ref ctx 43))
(define (ctx-fn-path-open ctx) (vector-ref ctx 44))
(define (ctx-fn-open-input-file ctx) (vector-ref ctx 45))
(define (ctx-fn-open-output-file ctx) (vector-ref ctx 46))
(define (ctx-fn-close-port ctx) (vector-ref ctx 47))
(define (ctx-fn-read-char-fn ctx) (vector-ref ctx 48))
(define (ctx-fn-peek-char-fn ctx) (vector-ref ctx 49))
(define (ctx-fn-write-char-fn ctx) (vector-ref ctx 50))
(define (ctx-ty-port ctx) (vector-ref ctx 51))
(define (ctx-ty-eof ctx) (vector-ref ctx 52))
(define (ctx-fn-substring ctx) (vector-ref ctx 53))
(define (ctx-fn-string-copy ctx) (vector-ref ctx 54))
(define (ctx-fn-string-to-list ctx) (vector-ref ctx 55))
(define (ctx-fn-list-to-string ctx) (vector-ref ctx 56))
(define (ctx-fn-string-eq ctx) (vector-ref ctx 57))
(define (ctx-fn-string-lt ctx) (vector-ref ctx 58))
(define (ctx-fn-string-ci-eq ctx) (vector-ref ctx 59))
(define (ctx-fn-string-ci-lt ctx) (vector-ref ctx 60))
(define (ctx-fn-read ctx) (vector-ref ctx 61))
(define (ctx-fn-read-list ctx) (vector-ref ctx 62))
(define (ctx-fn-read-string ctx) (vector-ref ctx 63))
(define (ctx-ty-vector ctx) (vector-ref ctx 64))
(define (ctx-fn-vector-copy ctx) (vector-ref ctx 65))
(define (ctx-fn-list-to-vector ctx) (vector-ref ctx 66))
(define (ctx-fn-vector-to-list ctx) (vector-ref ctx 80))
(define (ctx-fn-bv-alloc-fill ctx) (vector-ref ctx 67))
(define (ctx-fn-proc-exit ctx) (vector-ref ctx 68))
(define (ctx-fn-write ctx) (vector-ref ctx 69))
(define (ctx-fn-intern-sym ctx) (vector-ref ctx 70))
(define (ctx-boxed-vars ctx) (vector-ref ctx 71))
(define (ctx-boxed-locals ctx) (vector-ref ctx 72))
(define (ctx-fn-clock-time-get ctx) (vector-ref ctx 73))
(define (ctx-globals-hash ctx) (vector-ref ctx 74))
(define (ctx-funcs-hash ctx) (vector-ref ctx 75))
(define (ctx-locals-hash ctx) (vector-ref ctx 76))
(define (ctx-boxed-vars-hash ctx) (vector-ref ctx 77))
(define (ctx-boxed-locals-hash ctx) (vector-ref ctx 78))
(define (ctx-fn-apply ctx) (vector-ref ctx 79))
(define (ctx-fn-file-exists ctx) (vector-ref ctx 81))
(define (ctx-fn-bv-copy-range ctx) (vector-ref ctx 82))
(define (ctx-fn-bv-copy ctx) (vector-ref ctx 83))
(define (ctx-fn-bv-copy-from ctx) (vector-ref ctx 84))
(define (ctx-fn-bv-append ctx) (vector-ref ctx 85))
(define (ctx-fn-utf8-to-str-range ctx) (vector-ref ctx 86))
(define (ctx-fn-utf8-to-str ctx) (vector-ref ctx 87))
(define (ctx-fn-utf8-to-str-from ctx) (vector-ref ctx 88))
(define (ctx-fn-str-to-utf8-range ctx) (vector-ref ctx 89))
(define (ctx-fn-str-to-utf8 ctx) (vector-ref ctx 90))
(define (ctx-fn-str-to-utf8-from ctx) (vector-ref ctx 91))
(define (ctx-fn-environ-sizes-get ctx) (vector-ref ctx 92))
(define (ctx-fn-environ-get ctx) (vector-ref ctx 93))
(define (ctx-fn-get-env-var ctx) (vector-ref ctx 94))
(define (ctx-fn-get-env-vars ctx) (vector-ref ctx 95))
(define (ctx-fn-open-input-string ctx) (vector-ref ctx 96))
(define (ctx-fn-get-stdout ctx) (vector-ref ctx 97))
(define (ctx-fn-get-stderr ctx) (vector-ref ctx 98))
(define (ctx-fn-stream-write ctx) (vector-ref ctx 99))
(define (ctx-fn-clock-now ctx) (vector-ref ctx 100))
(define (ctx-fn-get-arguments ctx) (vector-ref ctx 101))
(define (ctx-fn-get-environment ctx) (vector-ref ctx 102))
(define (ctx-fn-get-stdin ctx) (vector-ref ctx 103))
(define (ctx-fn-stream-read ctx) (vector-ref ctx 104))
(define (ctx-fn-get-directories ctx) (vector-ref ctx 105))
(define (ctx-fn-open-at ctx) (vector-ref ctx 106))
(define (ctx-fn-read-via-stream ctx) (vector-ref ctx 107))
(define (ctx-fn-write-via-stream ctx) (vector-ref ctx 108))
(define (ctx-fn-drop-descriptor ctx) (vector-ref ctx 109))
(define (ctx-fn-drop-input-stream ctx) (vector-ref ctx 110))
(define (ctx-fn-drop-output-stream ctx) (vector-ref ctx 111))
(define (ctx-fn-vector-fill ctx) (vector-ref ctx 112))
(define (ctx-fn-string-fill ctx) (vector-ref ctx 113))
(define (ctx-ty-promise ctx) (vector-ref ctx 114))
(define (ctx-is-boxed? ctx name) (string-ht-has? (ctx-boxed-locals-hash ctx) name))

(define (build-boxed-locals-hash lst)
  (let ((ht (make-string-ht)))
    (let loop ((ls lst))
      (when (pair? ls)
        (string-ht-set! ht (car ls) #t)
        (loop (cdr ls))))
    ht))

(define (set-boxed-locals! ctx boxed-list)
  (vector-set! ctx 72 boxed-list)
  (vector-set! ctx 78 (build-boxed-locals-hash boxed-list)))

;; Check if a variable name appears as a set! target in an expression
(define (body-has-set? name expr)
  (and (pair? expr)
       (let ((head (car expr)))
         (cond
           ((not (symbol? head))
            (let loop ((es expr))
              (cond
                ((null? es) #f)
                ((not (pair? es)) #f)
                ((body-has-set? name (car es)) #t)
                (else (loop (cdr es))))))
           ((string=? (symbol->string head) "set!")
            (if (and (pair? (cdr expr)) (symbol? (cadr expr))
                     (string=? (symbol->string (cadr expr)) name))
                #t
                (let loop ((es (cddr expr)))
                  (cond
                    ((null? es) #f)
                    ((not (pair? es)) #f)
                    ((body-has-set? name (car es)) #t)
                    (else (loop (cdr es)))))))
           ((string=? (symbol->string head) "quote") #f)
           (else (let loop ((es expr))
                   (cond
                     ((null? es) #f)
                     ((not (pair? es)) #f)
                     ((body-has-set? name (car es)) #t)
                     (else (loop (cdr es))))))))))

;; Check if any form in a body list has set! on a given name
(define (body-list-has-set? name forms)
  (let loop ((fs forms))
    (cond
      ((null? fs) #f)
      ((body-has-set? name (car fs)) #t)
      (else (loop (cdr fs))))))

;; Collect ALL set! target names from a list of forms into a hash table (single pass)
(define (collect-set-targets forms)
  (let ((ht (make-string-ht)))
    (define (scan expr)
      (when (pair? expr)
        (let ((head (car expr)))
          (if (symbol? head)
              (let ((hs (symbol->string head)))
                (cond
                  ((string=? hs "set!")
                   (when (and (pair? (cdr expr)) (symbol? (cadr expr)))
                     (string-ht-set! ht (symbol->string (cadr expr)) #t))
                   (for-each scan (cddr expr)))
                  ((string=? hs "quote") #f)
                  (else (for-each scan (cdr expr)))))
              (for-each scan expr)))))
    (for-each scan forms)
    ht))

(define (build-locals-hash locals nlocals)
  (let ((ht (make-string-ht)))
    (let loop ((ls locals) (i 0))
      (when (and (< i nlocals) (pair? ls))
        (string-ht-set! ht (car ls) i)
        (loop (cdr ls) (+ i 1))))
    ht))

;; Cached hash tables for globals/funcs/boxed-vars (built once per compilation)
(define *cached-globals-hash* #f)
(define *cached-funcs-hash* #f)
(define *cached-boxed-vars-hash* #f)

(define (init-ctx-hashes! globals funcs boxed-vars)
  (let ((gh (make-string-ht))
        (fh (make-string-ht))
        (bh (make-string-ht)))
    (let loop ((gs globals))
      (when (pair? gs)
        (string-ht-set! gh (gv-name (car gs)) (gv-idx (car gs)))
        (loop (cdr gs))))
    (let loop ((fs funcs))
      (when (pair? fs)
        (when (uf-name (car fs))
          (string-ht-set! fh (uf-name (car fs)) (car fs)))
        (loop (cdr fs))))
    (let loop ((bv boxed-vars))
      (when (pair? bv)
        (string-ht-set! bh (car bv) #t)
        (loop (cdr bv))))
    (set! *cached-globals-hash* gh)
    (set! *cached-funcs-hash* fh)
    (set! *cached-boxed-vars-hash* bh)))

(define (make-ctx locals nlocals globals funcs lambdas wrappers
                  closure-arities nclosure-arity narity
                  fn-display fn-newline fn-eqv fn-equal
                  fn-num-to-str fn-str-to-num
                  fn-flonum-start fn-gcd fn-user-start ty-user-start
                  ty-flonum ty-rational ty-complex fn-make-complex fn-math-start
                  fn-command-line fn-string-append fn-linear-alloc fn-bv-alloc
                  fn-bv-copy-str ty-bytevector bump-ptr-idx fn-ptr-to-str
                  ty-char ty-eqv ty-symbol needs-rational needs-complex nfuncs
                  target
                  fn-open-input-file fn-open-output-file fn-close-port
                  fn-read-char-fn fn-peek-char-fn fn-write-char-fn
                  ty-port ty-eof
                  fn-substring fn-string-copy fn-string-to-list
                  fn-list-to-string fn-string-eq
                  fn-string-lt fn-string-ci-eq fn-string-ci-lt
                  fn-read fn-read-list fn-read-string
                  ty-vector fn-vector-copy fn-list-to-vector fn-vector-to-list
                  fn-bv-alloc-fill fn-proc-exit fn-write fn-intern-sym
                  boxed-vars fn-apply
                  fn-file-exists
                  fn-bv-copy-range fn-bv-copy fn-bv-copy-from
                  fn-bv-append
                  fn-utf8-to-str-range fn-utf8-to-str fn-utf8-to-str-from
                  fn-str-to-utf8-range fn-str-to-utf8 fn-str-to-utf8-from
                  fn-get-env-var fn-get-env-vars
                  fn-open-input-string
                  fn-get-stdout fn-get-stderr fn-stream-write
                  fn-clock-now fn-get-arguments fn-get-environment
                  fn-get-stdin fn-stream-read fn-get-directories
                  fn-open-at fn-read-via-stream fn-write-via-stream
                  fn-drop-descriptor fn-drop-input-stream fn-drop-output-stream
                  fn-vector-fill fn-string-fill
                  ty-promise)
  (vector locals nlocals globals funcs lambdas wrappers
          closure-arities nclosure-arity narity
          fn-display fn-newline fn-eqv fn-equal
          fn-num-to-str fn-str-to-num
          fn-flonum-start fn-gcd fn-user-start ty-user-start
          ty-flonum ty-rational ty-complex fn-make-complex fn-math-start
          fn-command-line fn-string-append fn-linear-alloc fn-bv-alloc
          fn-bv-copy-str ty-bytevector bump-ptr-idx fn-ptr-to-str
          ty-char ty-eqv ty-symbol needs-rational needs-complex nfuncs
          target 0 0 0
          0 0 0
          fn-open-input-file fn-open-output-file fn-close-port
          fn-read-char-fn fn-peek-char-fn fn-write-char-fn
          ty-port ty-eof
          fn-substring fn-string-copy fn-string-to-list
          fn-list-to-string fn-string-eq
          fn-string-lt fn-string-ci-eq fn-string-ci-lt
          fn-read fn-read-list fn-read-string
          ty-vector fn-vector-copy fn-list-to-vector
          fn-bv-alloc-fill fn-proc-exit fn-write fn-intern-sym
          boxed-vars '() 0
          *cached-globals-hash*
          *cached-funcs-hash*
          (build-locals-hash locals nlocals)
          *cached-boxed-vars-hash*
          (make-string-ht)
          fn-apply
          fn-vector-to-list
          fn-file-exists
          fn-bv-copy-range fn-bv-copy fn-bv-copy-from
          fn-bv-append
          fn-utf8-to-str-range fn-utf8-to-str fn-utf8-to-str-from
          fn-str-to-utf8-range fn-str-to-utf8 fn-str-to-utf8-from
          0 0
          fn-get-env-var fn-get-env-vars
          fn-open-input-string
          fn-get-stdout fn-get-stderr fn-stream-write
          fn-clock-now fn-get-arguments fn-get-environment
          fn-get-stdin fn-stream-read fn-get-directories
          fn-open-at fn-read-via-stream fn-write-via-stream
          fn-drop-descriptor fn-drop-input-stream fn-drop-output-stream
          fn-vector-fill fn-string-fill
          ty-promise))

(define (ctx-with-locals ctx locals nlocals)
  (let ((new-ctx (vector-copy ctx)))
    (vector-set! new-ctx 0 locals)
    (vector-set! new-ctx 1 nlocals)
    (vector-set! new-ctx 76 (build-locals-hash locals nlocals))
    new-ctx))

;; Extend ctx by appending new-names to its locals, incrementally updating the hash
(define (ctx-extend-locals ctx new-names)
  (let* ((old-locals (ctx-locals ctx))
         (old-nlocals (ctx-nlocals ctx))
         (locals (append old-locals new-names))
         (nlocals (+ old-nlocals (length new-names)))
         (new-ctx (vector-copy ctx))
         (ht (vector-copy (ctx-locals-hash ctx))))
    (let loop ((names new-names) (i old-nlocals))
      (when (pair? names)
        (let ((nm (car names)))
          (when (not (string=? nm ""))
            (string-ht-set! ht nm i)))
        (loop (cdr names) (+ i 1))))
    (vector-set! new-ctx 0 locals)
    (vector-set! new-ctx 1 nlocals)
    (vector-set! new-ctx 76 ht)
    new-ctx))

;;; --- Context lookup functions (hash-based) ---

(define (ctx-local ctx name)
  (string-ht-ref (ctx-locals-hash ctx) name -1))

(define (ctx-global ctx name)
  (string-ht-ref (ctx-globals-hash ctx) name -1))

(define (ctx-func ctx name)
  (string-ht-ref (ctx-funcs-hash ctx) name #f))

(define (ctx-lambda ctx node)
  (let loop ((ls (ctx-lambdas ctx)))
    (cond
      ((null? ls) #f)
      ((eq? (ll-form (car ls)) node) (car ls))
      (else (loop (cdr ls))))))

;;; --- String literal ---

(define (codegen-string str body)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (when (< i len)
        (wbuf-byte! body OP-I32-CONST)
        (wbuf-i32! body (char->integer (string-ref str i)))
        (loop (+ i 1))))
    (wbuf-byte! body OP-GC-PREFIX)
    (wbuf-byte! body GC-ARRAY-NEW-FIXED)
    (wbuf-u32! body TY-STRING)
    (wbuf-u32! body len)
    #t))

;;; --- Quote ---

(define (codegen-quote val body ctx)
  (cond
    ;; Boolean (must come before integer — booleans are i31 too)
    ((boolean? val)
     (wbuf-byte! body OP-I32-CONST)
     (wbuf-i32! body (if val 3 1))
     (emit-box-i31! body)
     #t)
    ;; Integer
    ((and (integer? val) (exact? val))
     (wbuf-byte! body OP-I32-CONST)
     (wbuf-i32! body (* val 2))
     (emit-box-i31! body)
     #t)
    ;; Flonum
    ((flonum? val)
     (wbuf-byte! body OP-F64-CONST)
     (wbuf-f64! body val)
     (emit-box-f64! body (ctx-ty-flonum ctx))
     #t)
    ;; Rational
    ((##ratnum? val)
     (wbuf-byte! body OP-I32-CONST)
     (wbuf-i32! body (##ratnum-numerator val))
     (wbuf-byte! body OP-I32-CONST)
     (wbuf-i32! body (##ratnum-denominator val))
     (emit-box-rational! body (ctx-ty-rational ctx))
     #t)
    ;; Complex
    ((##cpxnum? val)
     (and (codegen-quote (##cpxnum-real val) body ctx)
          (codegen-quote (##cpxnum-imag val) body ctx)
          (begin
            (wbuf-byte! body OP-CALL)
            (wbuf-u32! body (ctx-fn-make-complex ctx))
            #t)))
    ;; Character
    ((char? val)
     (wbuf-byte! body OP-I32-CONST)
     (wbuf-i32! body (char->integer val))
     (wbuf-byte! body OP-GC-PREFIX)
     (wbuf-byte! body GC-STRUCT-NEW)
     (wbuf-u32! body (ctx-ty-char ctx))
     #t)
    ;; String
    ((string? val)
     (codegen-string val body))
    ;; Empty list → null ref
    ((null? val)
     (wbuf-byte! body OP-REF-NULL)
     (wbuf-byte! body HT-EQ)
     #t)
    ;; Symbol → interned string wrapped in struct
    ((symbol? val)
     (let ((s (symbol->string val)))
       (let ((len (string-length s)))
         (let loop ((i 0))
           (when (< i len)
             (wbuf-byte! body OP-I32-CONST)
             (wbuf-i32! body (char->integer (string-ref s i)))
             (loop (+ i 1))))
         (wbuf-byte! body OP-GC-PREFIX)
         (wbuf-byte! body GC-ARRAY-NEW-FIXED)
         (wbuf-u32! body TY-STRING)
         (wbuf-u32! body len))
       (let ((fn-idx (ctx-fn-intern-sym ctx)))
         (if (>= fn-idx 0)
             (begin
               (wbuf-byte! body OP-CALL)
               (wbuf-u32! body fn-idx)
               #t)
             (begin
               (wbuf-byte! body OP-GC-PREFIX)
               (wbuf-byte! body GC-STRUCT-NEW)
               (wbuf-u32! body (ctx-ty-symbol ctx))
               #t)))))
    ;; List → recursive cons chain
    ((pair? val)
     (codegen-quote-list val body ctx))
    (else #f)))

(define (codegen-quote-list lst body ctx)
  (cond
    ((null? lst)
     (wbuf-byte! body OP-REF-NULL)
     (wbuf-byte! body HT-EQ)
     #t)
    ((not (pair? lst))
     ;; Dotted pair tail: emit the atom directly
     (codegen-quote lst body ctx))
    (else
     (and (codegen-quote (car lst) body ctx)
          (codegen-quote-list (cdr lst) body ctx)
          (begin
            (wbuf-byte! body OP-GC-PREFIX)
            (wbuf-byte! body GC-STRUCT-NEW)
            (wbuf-u32! body TY-PAIR)
            #t)))))

;;; --- Emit free var reference (local or global) ---

(define (emit-free-var! body ctx varname)
  (let ((li (ctx-local ctx varname)))
    (if (>= li 0)
        (begin
          (wbuf-byte! body OP-LOCAL-GET)
          (wbuf-u32! body li)
          #t)
        (let ((gi (ctx-global ctx varname)))
          (if (>= gi 0)
              (begin
                (wbuf-byte! body OP-GLOBAL-GET)
                (wbuf-u32! body gi)
                #t)
              (let ((fi (ctx-func ctx varname)))
                (if fi
                    ;; Known function name used as value — skip for now
                    (begin
                      (display (string-append "error: free var '" varname "' is func, not var\n")
                               (current-error-port))
                      #f)
                    (begin
                      (display (string-append "error: free var '" varname "' not found, nlocals=")
                               (current-error-port))
                      (display (ctx-nlocals ctx) (current-error-port))
                      (newline (current-error-port))
                      #f))))))))

;;; --- Main expression dispatch ---

(define (codegen-expr/tail val body ctx tail?)
  (cond
    ;; Boolean literal (must come before integer — booleans are i31 too)
    ((boolean? val)
     (wbuf-byte! body OP-I32-CONST)
     (wbuf-i32! body (if val 3 1))
     (emit-box-i31! body)
     #t)

    ;; Integer literal
    ((and (integer? val) (exact? val))
     (wbuf-byte! body OP-I32-CONST)
     (wbuf-i32! body (* val 2))
     (emit-box-i31! body)
     #t)

    ;; Flonum literal
    ((flonum? val)
     (wbuf-byte! body OP-F64-CONST)
     (wbuf-f64! body val)
     (emit-box-f64! body (ctx-ty-flonum ctx))
     #t)

    ;; Rational literal
    ((##ratnum? val)
     (wbuf-byte! body OP-I32-CONST)
     (wbuf-i32! body (##ratnum-numerator val))
     (wbuf-byte! body OP-I32-CONST)
     (wbuf-i32! body (##ratnum-denominator val))
     (emit-box-rational! body (ctx-ty-rational ctx))
     #t)

    ;; Complex literal
    ((##cpxnum? val)
     (and (codegen-expr (##cpxnum-real val) body ctx)
          (codegen-expr (##cpxnum-imag val) body ctx)
          (begin
            (wbuf-byte! body OP-CALL)
            (wbuf-u32! body (ctx-fn-make-complex ctx))
            #t)))

    ;; Character literal
    ((char? val)
     (wbuf-byte! body OP-I32-CONST)
     (wbuf-i32! body (char->integer val))
     (wbuf-byte! body OP-GC-PREFIX)
     (wbuf-byte! body GC-STRUCT-NEW)
     (wbuf-u32! body (ctx-ty-char ctx))
     #t)

    ;; String literal
    ((string? val)
     (codegen-string val body))

    ;; Symbol → variable reference
    ((symbol? val)
     (let ((name (symbol->string val)))
       (let ((idx (ctx-local ctx name)))
         (if (>= idx 0)
             (if (ctx-is-boxed? ctx name)
                 ;; Boxed var: local holds a 1-element array (box)
                 (begin
                   (wbuf-byte! body OP-LOCAL-GET) (wbuf-u32! body idx)
                   (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-REF-CAST) (wbuf-u32! body TY-ENV)
                   (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 0)
                   (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-ARRAY-GET)
                   (wbuf-u32! body TY-ENV) #t)
                 ;; Regular var
                 (begin
                   (wbuf-byte! body OP-LOCAL-GET)
                   (wbuf-u32! body idx)
                   #t))
             (let ((gidx (ctx-global ctx name)))
               (if (>= gidx 0)
                   (begin
                     (wbuf-byte! body OP-GLOBAL-GET)
                     (wbuf-u32! body gidx)
                     #t)
                   ;; function name as value → closure struct with wrapper
                   (let ((fn (ctx-func ctx name)))
                     (if fn
                         (let ((wtidx (let loop ((ws (ctx-wrappers ctx)))
                                        (cond
                                          ((null? ws) (uf-func-idx fn))
                                          ((= (vector-ref (car ws) 0) (uf-func-idx fn))
                                           (vector-ref (car ws) 1))
                                          (else (loop (cdr ws)))))))
                           (wbuf-byte! body OP-I32-CONST)
                           (wbuf-i32! body (- wtidx (ctx-fn-user-start ctx)))
                           (wbuf-byte! body OP-GC-PREFIX)
                           (wbuf-byte! body GC-ARRAY-NEW-FIXED)
                           (wbuf-u32! body TY-ENV)
                           (wbuf-u32! body 0)
                           (wbuf-byte! body OP-GC-PREFIX)
                           (wbuf-byte! body GC-STRUCT-NEW)
                           (wbuf-u32! body TY-CLOSURE)
                           #t)
                         (begin
                           (display (string-append "error: var '" name "' not found, nlocals=")
                                    (current-error-port))
                           (display (ctx-nlocals ctx) (current-error-port))
                           (display " nglobals=" (current-error-port))
                           (display (length (ctx-globals ctx)) (current-error-port))
                           (display " nfuncs=" (current-error-port))
                           (display (length (ctx-funcs ctx)) (current-error-port))
                           (display " locals=" (current-error-port))
                           (let loop ((ls (ctx-locals ctx)) (i 0))
                             (when (and (pair? ls) (< i (ctx-nlocals ctx)))
                               (when (> i 0) (display "," (current-error-port)))
                               (display (car ls) (current-error-port))
                               (loop (cdr ls) (+ i 1))))
                           (newline (current-error-port))
                           #f)))))))))

    ;; Empty list literal → nil
    ((null? val)
     (wbuf-byte! body OP-REF-NULL)
     (wbuf-byte! body HT-EQ)
     #t)

    ;; List form (application or special form)
    ((pair? val)
     (codegen-list-expr val body ctx tail?))

    (else
     (display "error: unknown expr type\n" (current-error-port))
     #f)))

(define (codegen-expr val body ctx)
  (codegen-expr/tail val body ctx #f))

;;; --- List expression dispatch ---

(define (codegen-list-expr val body ctx tail?)
  (let ((len (length val))
        (head (car val)))

    (cond
      ;; ((lambda (params...) body...) args...) — immediately applied lambda
      ((pair? head)
       (if (and (>= (length head) 3)
                (symbol? (car head))
                (string=? (symbol->string (car head)) "lambda")
                (list? (cadr head)))
           (let ((ll (ctx-lambda ctx head)))
             (if (not ll)
                 (begin
                   (display "error: unlifted lambda\n" (current-error-port))
                   #f)
                 (let ((ok #t))
                   ;; emit free vars (lifted extra params)
                   (let loop ((i 0) (fvs (ll-free-vars ll)))
                     (when (and ok (pair? fvs))
                       (set! ok (emit-free-var! body ctx (car fvs)))
                       (loop (+ i 1) (cdr fvs))))
                   ;; emit explicit args
                   (when ok
                     (let loop ((args (cdr val)))
                       (when (and ok (pair? args))
                         (set! ok (codegen-expr (car args) body ctx))
                         (loop (cdr args)))))
                   (when ok
                     (wbuf-byte! body (if tail? OP-RETURN-CALL OP-CALL))
                     (wbuf-u32! body (ll-func-idx ll)))
                   ok)))
           ;; dynamic call: head expression produces a value
           (let ((nargs (- len 1))
                 (ok #t))
             ;; emit args
             (let loop ((args (cdr val)))
               (when (and ok (pair? args))
                 (set! ok (codegen-expr (car args) body ctx))
                 (loop (cdr args))))
             ;; emit head
             (when ok
               (set! ok (codegen-expr head body ctx)))
             (when ok
               (emit-unbox-i31! body)
               ;; find type index for this arity
               (let ((tidx (let loop ((fs (ctx-funcs ctx)))
                             (cond
                               ((null? fs)
                                ;; try lambdas
                                (let loop2 ((ls (ctx-lambdas ctx)))
                                  (cond
                                    ((null? ls) -1)
                                    ((= (+ (ll-noriginal (car ls)) (ll-nfree (car ls))) nargs)
                                     (ll-type-idx (car ls)))
                                    (else (loop2 (cdr ls))))))
                               ((= (uf-nparams (car fs)) nargs) (uf-type-idx (car fs)))
                               (else (loop (cdr fs)))))))
                 (if (< tidx 0)
                     (begin
                       (display (string-append "error: no type for arity "
                                               (number->string nargs) "\n")
                                (current-error-port))
                       (set! ok #f))
                     (begin
                       (wbuf-byte! body (if tail? OP-RETURN-CALL-INDIRECT OP-CALL-INDIRECT))
                       (wbuf-u32! body tidx)
                       (wbuf-u32! body 0)))))
             ok)))

      ;; Not a symbol head → error
      ((not (symbol? head)) #f)

      ;; Symbol head → dispatch
      (else
       (let ((op head))
         (codegen-op-dispatch op val len body ctx tail?))))))

;;; --- Emit helpers (stack-level, no arg compilation) ---

(define (emit-ref-cast! body type-idx)
  (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-REF-CAST)
  (wbuf-u32! body type-idx))

(define (emit-cast-struct-get! body type-idx field-idx)
  (emit-ref-cast! body type-idx)
  (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-STRUCT-GET)
  (wbuf-u32! body type-idx) (wbuf-u32! body field-idx))

;;; --- Codegen helpers for common patterns ---

(define (codegen-binary-fixnum! opcode val body ctx)
  (and (codegen-i32-arg (cadr val) body ctx)
       (codegen-i32-arg (caddr val) body ctx)
       (begin (wbuf-byte! body opcode) (emit-box-fixnum! body) #t)))

(define (codegen-binary-bool! opcode val body ctx)
  (and (codegen-i32-arg (cadr val) body ctx)
       (codegen-i32-arg (caddr val) body ctx)
       (begin (wbuf-byte! body opcode) (emit-box-bool! body) #t)))

(define (codegen-binary-f64! opcode val body ctx)
  (and (codegen-f64-arg (cadr val) body ctx)
       (codegen-f64-arg (caddr val) body ctx)
       (begin (wbuf-byte! body opcode)
              (emit-box-f64! body (ctx-ty-flonum ctx)) #t)))

(define (codegen-unary-f64! opcode val body ctx)
  (and (codegen-f64-arg (cadr val) body ctx)
       (begin (wbuf-byte! body opcode)
              (emit-box-f64! body (ctx-ty-flonum ctx)) #t)))

(define (codegen-f64-cmp! opcode val body ctx)
  (and (codegen-f64-arg (cadr val) body ctx)
       (codegen-f64-arg (caddr val) body ctx)
       (begin (wbuf-byte! body opcode) (emit-box-bool! body) #t)))

(define (codegen-type-pred! type-idx val body ctx)
  (and (codegen-expr (cadr val) body ctx)
       (begin
         (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-REF-TEST-NN)
         (wbuf-u32! body type-idx)
         (emit-box-bool! body) #t)))

(define (codegen-struct-get! type-idx field-idx val body ctx)
  (and (codegen-expr (cadr val) body ctx)
       (begin (emit-cast-struct-get! body type-idx field-idx) #t)))

(define (codegen-call-1! fn-idx val body ctx)
  (and (codegen-expr (cadr val) body ctx)
       (begin (wbuf-byte! body OP-CALL) (wbuf-u32! body fn-idx) #t)))

(define (codegen-call-2! fn-idx val body ctx)
  (and (codegen-expr (cadr val) body ctx)
       (codegen-expr (caddr val) body ctx)
       (begin (wbuf-byte! body OP-CALL) (wbuf-u32! body fn-idx) #t)))

(define (codegen-call-3! fn-idx val body ctx)
  (and (codegen-expr (cadr val) body ctx)
       (codegen-expr (caddr val) body ctx)
       (codegen-expr (cadddr val) body ctx)
       (begin (wbuf-byte! body OP-CALL) (wbuf-u32! body fn-idx) #t)))

(define (codegen-struct-get-i31! type-idx field-idx val body ctx)
  (and (codegen-struct-get! type-idx field-idx val body ctx)
       (begin (emit-box-fixnum! body) #t)))

(define (codegen-struct-set-void! type-idx field-idx val body ctx)
  (and (codegen-expr (cadr val) body ctx)
       (begin
         (emit-ref-cast! body type-idx)
         (and (codegen-expr (caddr val) body ctx)
              (begin
                (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-STRUCT-SET)
                (wbuf-u32! body type-idx) (wbuf-u32! body field-idx)
                (emit-void! body) #t)))))

(define (codegen-array-len! type-idx val body ctx)
  (and (codegen-expr (cadr val) body ctx)
       (begin
         (emit-ref-cast! body type-idx)
         (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-ARRAY-LEN)
         (emit-box-fixnum! body) #t)))

(define (codegen-array-get! type-idx val body ctx)
  (and (codegen-expr (cadr val) body ctx)
       (begin
         (emit-ref-cast! body type-idx)
         (and (codegen-i32-arg (caddr val) body ctx)
              (begin
                (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-ARRAY-GET)
                (wbuf-u32! body type-idx) #t)))))

(define (codegen-array-set-void! type-idx val body ctx)
  (and (codegen-expr (cadr val) body ctx)
       (begin
         (emit-ref-cast! body type-idx)
         (and (codegen-i32-arg (caddr val) body ctx)
              (codegen-expr (cadddr val) body ctx)
              (begin
                (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-ARRAY-SET)
                (wbuf-u32! body type-idx)
                (emit-void! body) #t)))))

(define (codegen-maybe-type-pred! type-idx val body ctx)
  (and (codegen-expr (cadr val) body ctx)
       (begin
         (if (>= type-idx 0)
             (begin (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-REF-TEST-NN)
                    (wbuf-u32! body type-idx))
             (begin (wbuf-byte! body OP-DROP)
                    (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 0)))
         (emit-box-bool! body) #t)))

(define (codegen-i31-or-type-pred! type-idx val body ctx)
  (if (>= type-idx 0)
      (begin
        (wbuf-byte! body OP-BLOCK) (wbuf-byte! body TYPE-I32)
        (wbuf-byte! body OP-BLOCK) (wbuf-byte! body HT-EQ)
        (let ((ok (codegen-expr (cadr val) body ctx)))
          (when ok
            (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-BR-ON-CAST-FAIL)
            (wbuf-byte! body #x01) (wbuf-u32! body 0)
            (wbuf-byte! body HT-EQ) (wbuf-byte! body HT-I31)
            (wbuf-byte! body OP-DROP)
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 1)
            (wbuf-byte! body OP-BR) (wbuf-u32! body 1)
            (wbuf-byte! body OP-END)
            (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-REF-TEST-NN)
            (wbuf-u32! body type-idx)
            (wbuf-byte! body OP-END)
            (emit-box-bool! body))
          ok))
      (and (codegen-expr (cadr val) body ctx)
           (begin
             (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-REF-TEST-NN)
             (wbuf-byte! body HT-I31)
             (emit-box-bool! body) #t))))

(define (codegen-io-call! fn-idx val len body ctx)
  ;; display/write: 2-arg uses stdout, 3-arg extracts fd from port
  ;; WASI P2: fd is a stream handle from %get-stdout / port struct
  (and (codegen-expr (cadr val) body ctx)
       (if (= len 2)
           (begin
             (if (>= (ctx-fn-get-stdout ctx) 0)
                 (begin
                   (wbuf-byte! body OP-CALL) (wbuf-u32! body (ctx-fn-get-stdout ctx))
                   (emit-box-fixnum! body))
                 (begin
                   (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 1)
                   (emit-box-fixnum! body)))
             (wbuf-byte! body OP-CALL) (wbuf-u32! body fn-idx) #t)
           (if (>= (ctx-ty-port ctx) 0)
               ;; Port types exist: dispatch between port struct and raw i31
               (begin
                 (wbuf-byte! body OP-BLOCK) (wbuf-byte! body HT-EQ)
                 (and (codegen-expr (caddr val) body ctx)
                      (begin
                        (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-BR-ON-CAST-FAIL)
                        (wbuf-byte! body #x01) (wbuf-u32! body 0)
                        (wbuf-byte! body HT-EQ) (wbuf-u32! body (ctx-ty-port ctx))
                        ;; Cast succeeded: extract fd field and box as fixnum
                        (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-STRUCT-GET)
                        (wbuf-u32! body (ctx-ty-port ctx)) (wbuf-u32! body 0)
                        (emit-box-fixnum! body)
                        (wbuf-byte! body OP-END)
                        (wbuf-byte! body OP-CALL) (wbuf-u32! body fn-idx) #t)))
               ;; No port types: value is already i31 fd, pass through
               (and (codegen-expr (caddr val) body ctx)
                    (begin
                      (wbuf-byte! body OP-CALL) (wbuf-u32! body fn-idx) #t))))))

(define (codegen-port-mode-pred! mode val body ctx)
  ;; mode=0 → input-port? (eqz), mode=1 → output-port? (eq 1)
  (wbuf-byte! body OP-BLOCK) (wbuf-byte! body TYPE-I32)
  (wbuf-byte! body OP-BLOCK) (wbuf-byte! body HT-EQ)
  (let ((ok (codegen-expr (cadr val) body ctx)))
    (when ok
      (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-BR-ON-CAST-FAIL)
      (wbuf-byte! body #x01) (wbuf-u32! body 0)
      (wbuf-byte! body HT-EQ) (wbuf-u32! body (ctx-ty-port ctx))
      (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-STRUCT-GET)
      (wbuf-u32! body (ctx-ty-port ctx)) (wbuf-u32! body 1)
      (if (= mode 0)
          (wbuf-byte! body OP-I32-EQZ)
          (begin
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 1)
            (wbuf-byte! body OP-I32-EQ)))
      (wbuf-byte! body OP-BR) (wbuf-u32! body 1)
      (wbuf-byte! body OP-END)
      (wbuf-byte! body OP-DROP)
      (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 0)
      (wbuf-byte! body OP-END)
      (emit-box-bool! body))
    ok))

(define (codegen-complex-part! field-idx fallback-i31 val body ctx)
  ;; real-part (field 0, fallback=identity) or imag-part (field 1, fallback=0)
  (if (>= (ctx-ty-complex ctx) 0)
      (begin
        (wbuf-byte! body OP-BLOCK) (wbuf-byte! body HT-EQ)
        (wbuf-byte! body OP-BLOCK) (wbuf-byte! body HT-EQ)
        (let ((ok (codegen-expr (cadr val) body ctx)))
          (when ok
            (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-BR-ON-CAST-FAIL)
            (wbuf-byte! body #x01) (wbuf-u32! body 0)
            (wbuf-byte! body HT-EQ) (wbuf-u32! body (ctx-ty-complex ctx))
            (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-STRUCT-GET)
            (wbuf-u32! body (ctx-ty-complex ctx)) (wbuf-u32! body field-idx)
            (wbuf-byte! body OP-BR) (wbuf-u32! body 1)
            (wbuf-byte! body OP-END)
            (if (>= fallback-i31 0)
                (begin
                  (wbuf-byte! body OP-DROP)
                  (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body fallback-i31)
                  (emit-box-fixnum! body))
                'noop)
            (wbuf-byte! body OP-END))
          ok))
      (if (>= fallback-i31 0)
          (and (codegen-expr (cadr val) body ctx)
               (begin
                 (wbuf-byte! body OP-DROP)
                 (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body fallback-i31)
                 (emit-box-fixnum! body) #t))
          (codegen-expr (cadr val) body ctx))))

(define (codegen-bv-load! load-op val body ctx)
  ;; bytevector-u8-ref / bytevector-u32-native-ref
  (and (codegen-expr (cadr val) body ctx)
       (begin
         (emit-cast-struct-get! body (ctx-ty-bytevector ctx) 0)
         (and (codegen-expr (caddr val) body ctx)
              (begin
                (emit-unbox-fixnum! body)
                (wbuf-byte! body OP-I32-ADD)
                (wbuf-byte! body load-op) (wbuf-u32! body 0) (wbuf-u32! body 0)
                (emit-box-fixnum! body) #t)))))

(define (codegen-bv-store-i32! store-op val body ctx)
  ;; bytevector-u8-set! / bytevector-u32-native-set!
  (and (codegen-expr (cadr val) body ctx)
       (begin
         (emit-cast-struct-get! body (ctx-ty-bytevector ctx) 0)
         (and (codegen-expr (caddr val) body ctx)
              (begin
                (emit-unbox-fixnum! body)
                (wbuf-byte! body OP-I32-ADD)
                (and (codegen-expr (cadddr val) body ctx)
                     (begin
                       (emit-unbox-fixnum! body)
                       (wbuf-byte! body store-op) (wbuf-u32! body 0) (wbuf-u32! body 0)
                       (emit-void! body) #t)))))))

;;; --- Operator dispatch ---

;; Hash table of all known operators for fast rejection of function calls.
;; Lazily initialized since make-string-ht is in analyze.scm.
(define *codegen-ops* #f)
(define (init-codegen-ops!)
  (when (not *codegen-ops*)
    (let ((ht (make-string-ht)))
      (for-each (lambda (k) (string-ht-set! ht k #t))
        '("let" "let*" "lambda" "set!" "if" "begin" "quote" "and" "or" "cond"
          "cons" "car" "cdr" "set-car!" "set-cdr!" "null?" "pair?" "symbol?"
          "symbol->string" "string->symbol" "list" "pointer->string" "not"
          "boolean?" "number?" "complex?" "rational?" "real?" "integer?" "exact?"
          "inexact?" "flonum?" "##ratnum?" "##cpxnum?" "##ratnum-numerator"
          "##ratnum-denominator" "numerator" "denominator" "make-rectangular" "real-part" "##cpxnum-real"
          "imag-part" "##cpxnum-imag" "exact->inexact" "inexact->exact" "floor"
          "ceiling" "truncate" "round" "sqrt" "exp" "log" "sin" "cos" "tan"
          "asin" "acos" "atan" "expt" "zero?" "positive?" "negative?" "odd?"
          "even?" "eq?" "eqv?" "equal?" "number->string" "string->number" "display"
          "write" "current-error-port" "current-output-port" "current-input-port"
          "newline" "exit" "string-append"
          "command-line" "current-milliseconds" "linear-alloc" "make-bytevector"
          "bytevector-length" "bytevector-u8-ref" "bytevector-u8-set!"
          "bytevector-u32-native-ref" "bytevector-u32-native-set!"
          "bytevector-f64-native-set!" "##flonum->ieee754-64" "bytevector->pointer"
          "bytevector?" "bytevector-copy" "bytevector-append"
          "utf8->string" "string->utf8"
          "bytevector-copy-string!" "bytevector-copy!" "write-bytevector"
          "file-exists?" "open-input-file" "open-output-file" "open-input-string" "close-input-port"
          "close-output-port" "read-char" "peek-char" "write-char" "read"
          "char->integer" "char=?" "char<?" "char>?" "char<=?" "char>=?"
          "char-alphabetic?" "char-numeric?" "char-whitespace?"
          "char-upcase" "char-downcase"
          "char-ci=?" "char-ci<?" "char-ci>?" "char-ci<=?" "char-ci>=?"
          "integer->char" "eof-object?" "port?"
          "input-port?" "output-port?" "string?" "string-length" "string-ref"
          "string-set!" "make-string" "substring" "string-copy" "string->list"
          "list->string" "string=?" "string<?" "string-ci=?" "string-ci<?" "string-fill!"
          "vector?" "vector-length" "vector-ref"
          "vector-set!" "make-vector" "vector" "vector-copy" "list->vector" "vector->list" "vector-fill!" "+"
          "-" "*" "/" "quotient" "remainder" "=" "<" ">" "<=" ">="
          "%mem-store8" "%mem-store32" "%mem-load8" "%mem-load32" "%fd-write"
          "%i31-add" "%i31-sub" "%i31-mul" "%i31-div" "%i31-rem"
          "%i31-rem-u" "%i31-neg" "%i31-eqz" "%i31-eq" "%i31-lt" "%i31-gt"
          "%i31-le" "%i31-ge" "%i31-and" "%i31-xor" "%i31-or" "%i31-shl"
          "bitwise-and" "bitwise-ior" "arithmetic-shift" "%i31-div-u" "%i31-ge-u"
          "%i31-ne" "%i31?" "%flonum?" "%rational?" "%complex?" "char?" "%char?"
          "%string?" "%symbol?" "%pair?" "%car" "%cdr" "%ref-eq" "%ref-null"
          "%ref-is-null" "%f64-add" "%f64-sub" "%f64-mul" "%f64-div" "%f64-neg"
          "%f64-abs" "%f64-sqrt" "%f64-floor" "%f64-ceil" "%f64-trunc"
          "%f64-nearest" "%f64-copysign" "%f64-eq" "%f64-ne" "%f64-lt" "%f64-gt"
          "%f64-le" "%f64-ge" "%f64-convert-i31" "%f64-trunc-to-i31"
          "%rational-num" "%rational-den" "%make-rational" "%complex-real"
          "%complex-imag" "%make-complex-raw" "%char-code" "%make-char"
          "%symbol-string" "%string-length" "%string-ref" "%string-set!"
          "%make-string" "%vector-length" "%vector-ref" "%vector-set!"
          "%make-vector" "%memory-size" "%memory-grow" "%memory-fill"
          "%memory-copy" "%global-get-bump-ptr" "%global-set-bump-ptr!"
          "%wasi-args-sizes-get" "%wasi-args-get"
          "%wasi-environ-sizes-get" "%wasi-environ-get" "%wasi-fd-read"
          "%wasi-fd-close" "%wasi-path-open" "%make-port" "%port-fd" "%port-mode"
          "%port-buf" "%port-set-buf!" "%port-str" "%port-pos" "%port-set-pos!"
          "%port?" "%eof-new" "%eof?" "%unbox-f64"
          "%f64-ldexp" "%f64-exponent" "%f64-mantissa" "%make-flonum"
          "%make-bytevector-struct" "%bytevector-ptr" "%block-void"
          "%block-eqref" "%loop-void" "%br" "%br-if" "%local-set!" "%local-get"
          "%local-tee" "%unreachable" "%drop" "%if-i32" "%call"
          "for-each" "when" "unless" "char?" "procedure?" "apply"
          "emergency-exit" "get-environment-variable" "get-environment-variables"
          "%stream-write" "%get-stdout" "%get-stderr"
          "%call-get-arguments" "%call-get-environment"
          "%call-get-stdin" "%call-stream-read" "%call-get-directories"
          "%call-open-at" "%call-read-via-stream" "%call-write-via-stream"
          "%call-drop-descriptor" "%call-drop-input-stream" "%call-drop-output-stream"
          "%make-promise" "%promise-ref" "%promise-set!" "%promise-state" "%promise-set-state!" "promise?"))
      (set! *codegen-ops* ht))))

(define (codegen-op-dispatch op val len body ctx tail?)
 (let ((op-str (symbol->string op)))
  ;; Fast path: if op is not a known operator, it's a function call
  (if (not (string-ht-has? *codegen-ops* op-str))
      (codegen-function-call op-str val len body ctx tail?)
  (cond
    ;; Hot operators — dispatched early (inlined for forward-reference safety)

    ;; let
    ((and (string=? op-str "let") (>= len 3) (pair? (cadr val)))
     (let* ((bindings (cadr val))
            (nbinds (length bindings))
            (start (ctx-nlocals ctx))
            (bnames (map (lambda (b)
                           (if (and (pair? b) (symbol? (car b)))
                               (symbol->string (car b)) "_"))
                         bindings))
            (let-body (cddr val))
            (ok #t)
            (padding (let pad ((n nbinds) (acc '()))
                       (if (<= n 0) acc (pad (- n 1) (cons "" acc)))))
            (init-ctx (ctx-extend-locals ctx padding)))
       (let loop ((bs bindings) (j 0) (bn bnames))
         (when (and ok (pair? bs))
           (let ((b (car bs))
                 (bname (car bn)))
             (if (and (pair? b) (= (length b) 2))
                 (set! ok (codegen-expr (cadr b) body init-ctx))
                 (begin (emit-void! body)))
             (when ok
               (when (and (string-ht-has? (ctx-boxed-vars-hash ctx) bname)
                          (body-list-has-set? bname let-body))
                 (wbuf-byte! body OP-GC-PREFIX)
                 (wbuf-byte! body GC-ARRAY-NEW-FIXED)
                 (wbuf-u32! body TY-ENV)
                 (wbuf-u32! body 1))
               (wbuf-byte! body OP-LOCAL-SET)
               (wbuf-u32! body (+ start j))))
           (loop (cdr bs) (+ j 1) (cdr bn))))
       (when ok
         (let* ((new-boxed (filter (lambda (bn)
                                     (and (string-ht-has? (ctx-boxed-vars-hash ctx) bn)
                                          (body-list-has-set? bn let-body)))
                                   bnames))
                (new-ctx (ctx-extend-locals ctx bnames))
                (parent-boxed (filter (lambda (bl) (not (string-member bl bnames)))
                                     (ctx-boxed-locals ctx)))
                (merged-boxed (append new-boxed parent-boxed))
                (new-ctx (if (equal? merged-boxed (ctx-boxed-locals ctx))
                             new-ctx
                             (let ((c (vector-copy new-ctx)))
                               (set-boxed-locals! c merged-boxed)
                               c))))
           (set! ok (codegen-body/tail let-body body new-ctx tail?))))
       ok))

    ;; let*
    ((and (string=? op-str "let*") (>= len 3) (pair? (cadr val)))
     (let* ((bindings (cadr val))
            (nbinds (length bindings))
            (letstar-body (cddr val)))
       (if (= nbinds 0)
           (codegen-body/tail letstar-body body ctx tail?)
           (let* ((start (ctx-nlocals ctx))
                  (bnames (map (lambda (b)
                                 (if (and (pair? b) (symbol? (car b)))
                                     (symbol->string (car b)) "_"))
                               bindings))
                  (base-locals (ctx-locals ctx))
                  (total-nlocals (+ start nbinds))
                  (padding (let pad ((n nbinds) (acc '()))
                             (if (<= n 0) acc (pad (- n 1) (cons "" acc)))))
                  (full-locals (append base-locals padding))
                  (shared-ht (vector-copy (ctx-locals-hash ctx)))
                  (ok #t))
             (let loop ((bs bindings) (j 0) (bn bnames) (boxed-so-far '()))
               (when (and ok (pair? bs))
                 (let* ((b (car bs))
                        (bname (car bn))
                        (init-ctx (let ((c (vector-copy ctx)))
                                    (vector-set! c 0 full-locals)
                                    (vector-set! c 1 total-nlocals)
                                    (vector-set! c 76 shared-ht)
                                    (if (null? boxed-so-far)
                                        c
                                        (begin
                                          (set-boxed-locals! c (append boxed-so-far (ctx-boxed-locals ctx)))
                                          c)))))
                   (if (and (pair? b) (= (length b) 2))
                       (set! ok (codegen-expr (cadr b) body init-ctx))
                       (begin (emit-void! body)))
                   (let ((needs-box (and ok
                                        (string-ht-has? (ctx-boxed-vars-hash ctx) bname)
                                        (let ((remaining-inits
                                               (map cadr (filter (lambda (b) (and (pair? b) (= (length b) 2)))
                                                                 (cdr bs)))))
                                          (or (body-list-has-set? bname letstar-body)
                                              (body-list-has-set? bname remaining-inits))))))
                     (when ok
                       (when needs-box
                         (wbuf-byte! body OP-GC-PREFIX)
                         (wbuf-byte! body GC-ARRAY-NEW-FIXED)
                         (wbuf-u32! body TY-ENV)
                         (wbuf-u32! body 1))
                       (wbuf-byte! body OP-LOCAL-SET)
                       (wbuf-u32! body (+ start j)))
                     ;; Add this binding's name to shared hash for next iterations
                     (when (not (string=? bname ""))
                       (string-ht-set! shared-ht bname (+ start j)))
                     (loop (cdr bs) (+ j 1) (cdr bn)
                           (if needs-box (cons bname boxed-so-far) boxed-so-far))))))
             (when ok
               (let* ((all-let*-forms (append (map cadr (filter (lambda (b) (and (pair? b) (= (length b) 2)))
                                                                 bindings))
                                              letstar-body))
                      (new-boxed (filter (lambda (bn)
                                           (and (string-ht-has? (ctx-boxed-vars-hash ctx) bn)
                                                (body-list-has-set? bn all-let*-forms)))
                                         bnames))
                      (full-ctx (ctx-extend-locals ctx bnames))
                      (parent-boxed (filter (lambda (bl) (not (string-member bl bnames)))
                                           (ctx-boxed-locals ctx)))
                      (merged-boxed (append new-boxed parent-boxed))
                      (full-ctx (if (equal? merged-boxed (ctx-boxed-locals ctx))
                                    full-ctx
                                    (let ((c (vector-copy full-ctx)))
                                      (set-boxed-locals! c merged-boxed)
                                      c))))
                 (set! ok (codegen-body/tail letstar-body body full-ctx tail?))))
             ok))))

    ;; lambda
    ((and (string=? op-str "lambda") (>= len 3) (list? (cadr val)))
     (let ((ll (ctx-lambda ctx val)))
       (if (not ll)
           (begin
             (display "error: unlifted lambda\n" (current-error-port))
             #f)
           (let ((ok #t))
             (wbuf-byte! body OP-I32-CONST)
             (wbuf-i32! body (- (ll-func-idx ll) (ctx-fn-user-start ctx)))
             (let loop ((fvs (ll-free-vars ll)))
               (when (and ok (pair? fvs))
                 (set! ok (emit-free-var! body ctx (car fvs)))
                 (loop (cdr fvs))))
             (when ok
               (wbuf-byte! body OP-GC-PREFIX)
               (wbuf-byte! body GC-ARRAY-NEW-FIXED)
               (wbuf-u32! body TY-ENV)
               (wbuf-u32! body (ll-nfree ll))
               (wbuf-byte! body OP-GC-PREFIX)
               (wbuf-byte! body GC-STRUCT-NEW)
               (wbuf-u32! body TY-CLOSURE))
             ok))))

    ;; set!
    ((and (string=? op-str "set!") (= len 3) (symbol? (cadr val)))
     (let* ((vname (symbol->string (cadr val)))
            (li (ctx-local ctx vname)))
       (cond
         ((and (>= li 0) (ctx-is-boxed? ctx vname))
          (wbuf-byte! body OP-LOCAL-GET) (wbuf-u32! body li)
          (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-REF-CAST) (wbuf-u32! body TY-ENV)
          (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 0)
          (and (codegen-expr (caddr val) body ctx)
               (begin
                 (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-ARRAY-SET)
                 (wbuf-u32! body TY-ENV)
                 (emit-void! body) #t)))
         ((>= li 0)
          (and (codegen-expr (caddr val) body ctx)
               (begin
                 (wbuf-byte! body OP-LOCAL-SET) (wbuf-u32! body li)
                 (emit-void! body) #t)))
         (else
          (let ((gi (ctx-global ctx vname)))
            (if (< gi 0)
                (begin
                  (display (string-append "error: set! unknown var '" vname "'\n")
                           (current-error-port))
                  #f)
                (and (codegen-expr (caddr val) body ctx)
                     (begin
                       (wbuf-byte! body OP-GLOBAL-SET) (wbuf-u32! body gi)
                       (emit-void! body) #t))))))))
    ;; if
    ((and (string=? op-str "if") (or (= len 3) (= len 4)))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-truthy-test! body)
            (wbuf-byte! body OP-IF)
            (wbuf-byte! body HT-EQ)
            (and (codegen-expr/tail (caddr val) body ctx tail?)
                 (begin
                   (wbuf-byte! body OP-ELSE)
                   (if (= len 4)
                       (codegen-expr/tail (cadddr val) body ctx tail?)
                       (begin
                         (emit-void! body)
                         #t))
                   (wbuf-byte! body OP-END)
                   #t)))))

    ;; begin
    ((and (string=? op-str "begin") (>= len 2))
     (codegen-body/tail (cdr val) body ctx tail?))

    ;; quote
    ((and (string=? op-str "quote") (= len 2))
     (codegen-quote (cadr val) body ctx))

    ;; and
    ((string=? op-str "and")
     (cond
       ((= len 1)
        (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 3)
        (emit-box-i31! body) #t)
       ((= len 2)
        (codegen-expr/tail (cadr val) body ctx tail?))
       (else
        (let ((nexprs (- len 1))
              (ok #t))
          (let loop ((i 0) (exprs (cdr val)))
            (when (and ok (< i (- nexprs 1)) (pair? exprs))
              (set! ok (codegen-expr (car exprs) body ctx))
              (when ok
                (emit-truthy-test! body)
                (wbuf-byte! body OP-IF)
                (wbuf-byte! body HT-EQ))
              (loop (+ i 1) (cdr exprs))))
          (when ok
            (set! ok (codegen-expr/tail (list-ref val nexprs) body ctx tail?)))
          (when ok
            (let loop ((i 0))
              (when (< i (- nexprs 1))
                (wbuf-byte! body OP-ELSE)
                (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 1)
                (emit-box-i31! body)
                (wbuf-byte! body OP-END)
                (loop (+ i 1)))))
          ok))))

    ;; or
    ((string=? op-str "or")
     (cond
       ((= len 1)
        (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 1)
        (emit-box-i31! body) #t)
       ((= len 2)
        (codegen-expr/tail (cadr val) body ctx tail?))
       (else
        ;; Use a temporary local to preserve the value for truthy results.
        ;; Allocate a local at current nlocals position.
        (let* ((tmp-idx (ctx-nlocals ctx))
               (new-ctx (ctx-extend-locals ctx (list "%or-tmp")))
               (nexprs (- len 1))
               (ok #t))
          (let loop ((i 0) (exprs (cdr val)))
            (when (and ok (< i (- nexprs 1)) (pair? exprs))
              (set! ok (codegen-expr (car exprs) body new-ctx))
              (when ok
                ;; Save value to temp local
                (wbuf-byte! body OP-LOCAL-TEE) (wbuf-u32! body tmp-idx)
                ;; Test truthiness
                (emit-truthy-test! body)
                (wbuf-byte! body OP-IF)
                (wbuf-byte! body HT-EQ)
                ;; Truthy: return the saved value
                (wbuf-byte! body OP-LOCAL-GET) (wbuf-u32! body tmp-idx)
                (wbuf-byte! body OP-ELSE))
              (loop (+ i 1) (cdr exprs))))
          (when ok
            (set! ok (codegen-expr/tail (list-ref val nexprs) body new-ctx tail?)))
          (when ok
            (let loop ((i 0))
              (when (< i (- nexprs 1))
                (wbuf-byte! body OP-END)
                (loop (+ i 1)))))
          ok))))

    ;; cond
    ((and (string=? op-str "cond") (>= len 2))
     (let ((nclauses (- len 1))
           (nifs 0)
           (ok #t))
       (let loop ((i 0) (clauses (cdr val)))
         (when (and ok (pair? clauses))
           (let ((clause (car clauses)))
             (if (not (and (pair? clause) (>= (length clause) 2)))
                 (set! ok #f)
                 (if (and (symbol? (car clause)) (string=? (symbol->string (car clause)) "else"))
                     ;; else clause
                     (set! ok (codegen-body/tail (cdr clause) body ctx tail?))
                     (begin
                       (set! ok (codegen-expr (car clause) body ctx))
                       (when ok
                         (emit-truthy-test! body)
                         (wbuf-byte! body OP-IF)
                         (wbuf-byte! body HT-EQ)
                         (set! ok (codegen-body/tail (cdr clause) body ctx tail?))
                         (set! nifs (+ nifs 1))
                         (when (and ok (< i (- nclauses 1)))
                           (wbuf-byte! body OP-ELSE))
                         (when (and ok (= i (- nclauses 1)))
                           ;; last clause, no else: default to void
                           (wbuf-byte! body OP-ELSE)
                           (emit-void! body)))
                       (loop (+ i 1) (cdr clauses))))))))
       (when ok
         (let loop ((i 0))
           (when (< i nifs)
             (wbuf-byte! body OP-END)
             (loop (+ i 1)))))
       ok))

    ;; cons
    ((and (string=? op-str "cons") (= len 3))
     (and (codegen-expr (cadr val) body ctx)
          (codegen-expr (caddr val) body ctx)
          (begin
            (wbuf-byte! body OP-GC-PREFIX)
            (wbuf-byte! body GC-STRUCT-NEW)
            (wbuf-u32! body TY-PAIR)
            #t)))

    ((and (string=? op-str "car") (= len 2)) (codegen-struct-get! TY-PAIR 0 val body ctx))
    ((and (string=? op-str "cdr") (= len 2)) (codegen-struct-get! TY-PAIR 1 val body ctx))

    ((and (string=? op-str "set-car!") (= len 3)) (codegen-struct-set-void! TY-PAIR 0 val body ctx))
    ((and (string=? op-str "set-cdr!") (= len 3)) (codegen-struct-set-void! TY-PAIR 1 val body ctx))

    ;; null?
    ((and (string=? op-str "null?") (= len 2))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (wbuf-byte! body OP-REF-IS-NULL)
            (emit-box-bool! body)
            #t)))

    ((and (string=? op-str "pair?") (= len 2)) (codegen-type-pred! TY-PAIR val body ctx))
    ((and (string=? op-str "procedure?") (= len 2)) (codegen-type-pred! TY-CLOSURE val body ctx))
    ((and (string=? op-str "symbol?") (= len 2)) (codegen-type-pred! (ctx-ty-symbol ctx) val body ctx))

    ((and (string=? op-str "symbol->string") (= len 2)) (codegen-struct-get! (ctx-ty-symbol ctx) 0 val body ctx))

    ;; string->symbol
    ((and (string=? op-str "string->symbol") (= len 2))
     (and (codegen-expr (cadr val) body ctx)
          (let ((fn-idx (ctx-fn-intern-sym ctx)))
            (if (>= fn-idx 0)
                (begin
                  (wbuf-byte! body OP-CALL)
                  (wbuf-u32! body fn-idx)
                  #t)
                (begin
                  (wbuf-byte! body OP-GC-PREFIX)
                  (wbuf-byte! body GC-REF-CAST-NULL)
                  (wbuf-u32! body TY-STRING)
                  (wbuf-byte! body OP-GC-PREFIX)
                  (wbuf-byte! body GC-STRUCT-NEW)
                  (wbuf-u32! body (ctx-ty-symbol ctx))
                  #t)))))

    ;; list
    ((string=? op-str "list")
     (let ((nargs (- len 1))
           (ok #t))
       (let loop ((args (cdr val)))
         (when (and ok (pair? args))
           (set! ok (codegen-expr (car args) body ctx))
           (loop (cdr args))))
       (when ok
         (wbuf-byte! body OP-REF-NULL)
         (wbuf-byte! body HT-EQ)
         (let loop ((i 0))
           (when (< i nargs)
             (wbuf-byte! body OP-GC-PREFIX)
             (wbuf-byte! body GC-STRUCT-NEW)
             (wbuf-u32! body TY-PAIR)
             (loop (+ i 1)))))
       ok))

    ((and (string=? op-str "pointer->string") (= len 3))
     (codegen-call-2! (ctx-fn-ptr-to-str ctx) val body ctx))

    ;; not
    ((and (string=? op-str "not") (= len 2))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-truthy-test! body)
            (wbuf-byte! body OP-I32-EQZ)
            (emit-box-bool! body)
            #t)))

    ;; boolean? — must handle any value type, not just i31
    ((and (string=? op-str "boolean?") (= len 2))
     (begin
       ;; block (result i32) — outer
       (wbuf-byte! body OP-BLOCK) (wbuf-byte! body TYPE-I32)
       ;;   block (result eqref) — inner (for cast fail path)
       (wbuf-byte! body OP-BLOCK) (wbuf-byte! body HT-EQ)
       ;;     compile argument INSIDE the blocks
       (let ((ok (codegen-expr (cadr val) body ctx)))
         (when ok
           ;;     br_on_cast_fail 0 eq i31 — if not i31, branch to inner
           (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-BR-ON-CAST-FAIL)
           (wbuf-byte! body #x01) (wbuf-u32! body 0)
           (wbuf-byte! body HT-EQ) (wbuf-byte! body HT-I31)
           ;;     fall through: value is i31 on stack
           (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-I31-GET-S)
           (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 2)
           (wbuf-byte! body OP-I32-OR)
           (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 3)
           (wbuf-byte! body OP-I32-EQ)
           (wbuf-byte! body OP-BR) (wbuf-u32! body 1) ;; br outer with i32 result
           ;;   end inner — non-i31 value on stack
           (wbuf-byte! body OP-END)
           (wbuf-byte! body OP-DROP) ;; drop non-i31 value
           (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 0) ;; false
           ;; end outer — i32 result
           (wbuf-byte! body OP-END)
           (emit-box-bool! body))
         ok)))

    ;; number? / complex?
    ((and (or (string=? op-str "number?") (string=? op-str "complex?")) (= len 2))
     (let ((has-fl (>= (ctx-ty-flonum ctx) 0))
           (has-rat (>= (ctx-ty-rational ctx) 0))
           (has-cx (>= (ctx-ty-complex ctx) 0)))
       (if (or has-fl has-rat has-cx)
           (let ((types '())
                 (ntypes 0))
             (when has-fl
               (set! types (append types (list (ctx-ty-flonum ctx))))
               (set! ntypes (+ ntypes 1)))
             (when has-rat
               (set! types (append types (list (ctx-ty-rational ctx))))
               (set! ntypes (+ ntypes 1)))
             (when has-cx
               (set! types (append types (list (ctx-ty-complex ctx))))
               (set! ntypes (+ ntypes 1)))
             (wbuf-byte! body OP-BLOCK) (wbuf-byte! body TYPE-I32)
             (let loop ((t 0))
               (when (< t ntypes)
                 (wbuf-byte! body OP-BLOCK) (wbuf-byte! body HT-EQ)
                 (loop (+ t 1))))
             (let ((ok (codegen-expr (cadr val) body ctx)))
               (when ok
                 ;; i31 test
                 (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-BR-ON-CAST-FAIL)
                 (wbuf-byte! body #x01) (wbuf-u32! body 0)
                 (wbuf-byte! body HT-EQ) (wbuf-byte! body HT-I31)
                 (wbuf-byte! body OP-DROP)
                 (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 1)
                 (wbuf-byte! body OP-BR) (wbuf-u32! body ntypes)
                 (wbuf-byte! body OP-END)
                 ;; intermediate type tests
                 (let loop ((t 0) (ts types))
                   (when (< t (- ntypes 1))
                     (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-BR-ON-CAST-FAIL)
                     (wbuf-byte! body #x01) (wbuf-u32! body 0)
                     (wbuf-byte! body HT-EQ) (wbuf-u32! body (car ts))
                     (wbuf-byte! body OP-DROP)
                     (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 1)
                     (wbuf-byte! body OP-BR) (wbuf-u32! body (- ntypes 1 t))
                     (wbuf-byte! body OP-END)
                     (loop (+ t 1) (cdr ts))))
                 ;; final type: ref.test
                 (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-REF-TEST-NN)
                 (wbuf-u32! body (list-ref types (- ntypes 1)))
                 (wbuf-byte! body OP-END)
                 (emit-box-bool! body))
               ok))
           ;; no numeric types beyond i31
           (and (codegen-expr (cadr val) body ctx)
                (begin
                  (wbuf-byte! body OP-GC-PREFIX)
                  (wbuf-byte! body GC-REF-TEST-NN)
                  (wbuf-byte! body HT-I31)
                  (emit-box-bool! body)
                  #t)))))

    ((and (string=? op-str "rational?") (= len 2))
     (codegen-i31-or-type-pred! (ctx-ty-rational ctx) val body ctx))

    ((and (string=? op-str "real?") (= len 2))
     ;; true for i31, flonum, rational — false for complex
     (let ((has-fl (>= (ctx-ty-flonum ctx) 0))
           (has-rat (>= (ctx-ty-rational ctx) 0)))
       (if (or has-fl has-rat)
           (let ((types '())
                 (ntypes 0))
             (when has-fl
               (set! types (append types (list (ctx-ty-flonum ctx))))
               (set! ntypes (+ ntypes 1)))
             (when has-rat
               (set! types (append types (list (ctx-ty-rational ctx))))
               (set! ntypes (+ ntypes 1)))
             (wbuf-byte! body OP-BLOCK) (wbuf-byte! body TYPE-I32)
             (let loop ((t 0))
               (when (< t ntypes)
                 (wbuf-byte! body OP-BLOCK) (wbuf-byte! body HT-EQ)
                 (loop (+ t 1))))
             (let ((ok (codegen-expr (cadr val) body ctx)))
               (when ok
                 ;; i31 test
                 (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-BR-ON-CAST-FAIL)
                 (wbuf-byte! body #x01) (wbuf-u32! body 0)
                 (wbuf-byte! body HT-EQ) (wbuf-byte! body HT-I31)
                 (wbuf-byte! body OP-DROP)
                 (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 1)
                 (wbuf-byte! body OP-BR) (wbuf-u32! body ntypes)
                 (wbuf-byte! body OP-END)
                 ;; intermediate type tests
                 (let loop ((t 0) (ts types))
                   (when (< t (- ntypes 1))
                     (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-BR-ON-CAST-FAIL)
                     (wbuf-byte! body #x01) (wbuf-u32! body 0)
                     (wbuf-byte! body HT-EQ) (wbuf-u32! body (car ts))
                     (wbuf-byte! body OP-DROP)
                     (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 1)
                     (wbuf-byte! body OP-BR) (wbuf-u32! body (- ntypes 1 t))
                     (wbuf-byte! body OP-END)
                     (loop (+ t 1) (cdr ts))))
                 ;; final type: ref.test
                 (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-REF-TEST-NN)
                 (wbuf-u32! body (list-ref types (- ntypes 1)))
                 (wbuf-byte! body OP-END)
                 (emit-box-bool! body))
               ok))
           ;; no flonum/rational: real? = integer?
           (and (codegen-expr (cadr val) body ctx)
                (begin
                  (wbuf-byte! body OP-GC-PREFIX)
                  (wbuf-byte! body GC-REF-TEST-NN)
                  (wbuf-byte! body HT-I31)
                  (emit-box-bool! body)
                  #t)))))

    ;; numerator: i31 → self; rational → field 0
    ((and (string=? op-str "numerator") (= len 2))
     (let ((has-rat (>= (ctx-ty-rational ctx) 0)))
       (if (not has-rat)
           (codegen-expr (cadr val) body ctx)
           (begin
             (wbuf-byte! body OP-BLOCK) (wbuf-byte! body HT-EQ)
             (wbuf-byte! body OP-BLOCK) (wbuf-byte! body HT-EQ)
             (let ((ok (codegen-expr (cadr val) body ctx)))
               (when ok
                 (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-BR-ON-CAST-FAIL)
                 (wbuf-byte! body #x01) (wbuf-u32! body 0)
                 (wbuf-byte! body HT-EQ) (wbuf-byte! body HT-I31)
                 ;; IS i31: numerator = self
                 (wbuf-byte! body OP-BR) (wbuf-u32! body 1)
                 (wbuf-byte! body OP-END)
                 ;; NOT i31: get rational numerator (field 0)
                 (emit-cast-struct-get! body (ctx-ty-rational ctx) 0)
                 (emit-box-fixnum! body)
                 (wbuf-byte! body OP-END))
               ok)))))

    ;; denominator: i31 → 1; rational → field 1
    ((and (string=? op-str "denominator") (= len 2))
     (let ((has-rat (>= (ctx-ty-rational ctx) 0)))
       (if (not has-rat)
           ;; no rationals: denominator of integer is always 1
           (and (codegen-expr (cadr val) body ctx)
                (begin
                  (wbuf-byte! body OP-DROP)
                  (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 1)
                  (emit-box-fixnum! body) #t))
           (begin
             (wbuf-byte! body OP-BLOCK) (wbuf-byte! body HT-EQ)
             (wbuf-byte! body OP-BLOCK) (wbuf-byte! body HT-EQ)
             (let ((ok (codegen-expr (cadr val) body ctx)))
               (when ok
                 (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-BR-ON-CAST-FAIL)
                 (wbuf-byte! body #x01) (wbuf-u32! body 0)
                 (wbuf-byte! body HT-EQ) (wbuf-byte! body HT-I31)
                 ;; IS i31: denominator = 1
                 (wbuf-byte! body OP-DROP)
                 (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 1)
                 (emit-box-fixnum! body)
                 (wbuf-byte! body OP-BR) (wbuf-u32! body 1)
                 (wbuf-byte! body OP-END)
                 ;; NOT i31: get rational denominator (field 1)
                 (emit-cast-struct-get! body (ctx-ty-rational ctx) 1)
                 (emit-box-fixnum! body)
                 (wbuf-byte! body OP-END))
               ok)))))

    ((and (string=? op-str "integer?") (= len 2)) (codegen-type-pred! HT-I31 val body ctx))

    ((and (string=? op-str "exact?") (= len 2))
     (codegen-i31-or-type-pred! (ctx-ty-rational ctx) val body ctx))

    ((and (string=? op-str "inexact?") (= len 2)) (codegen-maybe-type-pred! (ctx-ty-flonum ctx) val body ctx))

    ((and (string=? op-str "flonum?") (= len 2)) (codegen-maybe-type-pred! (ctx-ty-flonum ctx) val body ctx))

    ((and (string=? op-str "##ratnum?") (= len 2)) (codegen-maybe-type-pred! (ctx-ty-rational ctx) val body ctx))
    ((and (string=? op-str "##cpxnum?") (= len 2)) (codegen-maybe-type-pred! (ctx-ty-complex ctx) val body ctx))

    ((and (string=? op-str "##ratnum-numerator") (= len 2)) (codegen-struct-get-i31! (ctx-ty-rational ctx) 0 val body ctx))
    ((and (string=? op-str "##ratnum-denominator") (= len 2)) (codegen-struct-get-i31! (ctx-ty-rational ctx) 1 val body ctx))

    ((and (string=? op-str "make-rectangular") (= len 3))
     (codegen-call-2! (ctx-fn-make-complex ctx) val body ctx))

    ((and (or (string=? op-str "real-part") (string=? op-str "##cpxnum-real")) (= len 2))
     (codegen-complex-part! 0 -1 val body ctx))

    ((and (or (string=? op-str "imag-part") (string=? op-str "##cpxnum-imag")) (= len 2))
     (codegen-complex-part! 1 0 val body ctx))

    ;; exact->inexact
    ((and (string=? op-str "exact->inexact") (= len 2))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (when (>= (ctx-fn-flonum-start ctx) 0)
              (wbuf-byte! body OP-CALL)
              (wbuf-u32! body (+ (ctx-fn-flonum-start ctx) FL-EXACT-TO-INEXACT)))
            #t)))

    ;; inexact->exact
    ((and (string=? op-str "inexact->exact") (= len 2))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (when (>= (ctx-fn-flonum-start ctx) 0)
              (wbuf-byte! body OP-CALL)
              (wbuf-u32! body (+ (ctx-fn-flonum-start ctx) FL-INEXACT-TO-EXACT)))
            #t)))

    ;; floor, ceiling, truncate, round
    ((and (or (string=? op-str "floor") (string=? op-str "ceiling") (string=? op-str "truncate") (string=? op-str "round"))
          (= len 2))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (when (>= (ctx-fn-flonum-start ctx) 0)
              (let ((offset (cond ((string=? op-str "floor") FL-FLOOR)
                                  ((string=? op-str "ceiling") FL-CEILING)
                                  ((string=? op-str "truncate") FL-TRUNCATE)
                                  (else FL-ROUND))))
                (wbuf-byte! body OP-CALL)
                (wbuf-u32! body (+ (ctx-fn-flonum-start ctx) offset))))
            #t)))

    ;; unary math: sqrt, exp, log, sin, cos, tan, asin, acos
    ((and (>= (ctx-fn-math-start ctx) 0) (= len 2)
          (let ((mo (cond ((string=? op-str "sqrt") MATH-SQRT)
                          ((string=? op-str "exp") MATH-EXP)
                          ((string=? op-str "log") MATH-LOG)
                          ((string=? op-str "sin") MATH-SIN)
                          ((string=? op-str "cos") MATH-COS)
                          ((string=? op-str "tan") MATH-TAN)
                          ((string=? op-str "asin") MATH-ASIN)
                          ((string=? op-str "acos") MATH-ACOS)
                          (else -1))))
            (>= mo 0)))
     (and (codegen-expr (cadr val) body ctx)
          (let ((mo (cond ((string=? op-str "sqrt") MATH-SQRT)
                          ((string=? op-str "exp") MATH-EXP)
                          ((string=? op-str "log") MATH-LOG)
                          ((string=? op-str "sin") MATH-SIN)
                          ((string=? op-str "cos") MATH-COS)
                          ((string=? op-str "tan") MATH-TAN)
                          ((string=? op-str "asin") MATH-ASIN)
                          ((string=? op-str "acos") MATH-ACOS)
                          (else -1))))
            (wbuf-byte! body OP-CALL)
            (wbuf-u32! body (+ (ctx-fn-math-start ctx) mo))
            #t)))

    ;; atan: 1-arg or 2-arg
    ((and (>= (ctx-fn-math-start ctx) 0) (string=? op-str "atan"))
     (cond
       ((= len 2)
        (and (codegen-expr (cadr val) body ctx)
             (begin
               (wbuf-byte! body OP-CALL)
               (wbuf-u32! body (+ (ctx-fn-math-start ctx) MATH-ATAN))
               #t)))
       ((= len 3)
        (and (codegen-expr (cadr val) body ctx)
             (codegen-expr (caddr val) body ctx)
             (begin
               (wbuf-byte! body OP-CALL)
               (wbuf-u32! body (+ (ctx-fn-math-start ctx) MATH-ATAN2))
               #t)))
       (else #f)))

    ;; expt
    ((and (>= (ctx-fn-math-start ctx) 0) (string=? op-str "expt") (= len 3))
     (and (codegen-expr (cadr val) body ctx)
          (codegen-expr (caddr val) body ctx)
          (begin
            (wbuf-byte! body OP-CALL)
            (wbuf-u32! body (+ (ctx-fn-math-start ctx) MATH-EXPT))
            #t)))

    ;; zero?
    ((and (string=? op-str "zero?") (= len 2))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-unbox-fixnum! body)
            (wbuf-byte! body OP-I32-EQZ)
            (emit-box-bool! body)
            #t)))

    ;; positive?
    ((and (string=? op-str "positive?") (= len 2))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-unbox-fixnum! body)
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 0)
            (wbuf-byte! body OP-I32-GT-S)
            (emit-box-bool! body)
            #t)))

    ;; negative?
    ((and (string=? op-str "negative?") (= len 2))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-unbox-fixnum! body)
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 0)
            (wbuf-byte! body OP-I32-LT-S)
            (emit-box-bool! body)
            #t)))

    ;; odd?
    ((and (string=? op-str "odd?") (= len 2))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-unbox-fixnum! body)
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 1)
            (wbuf-byte! body OP-I32-AND)
            (emit-box-bool! body)
            #t)))

    ;; even?
    ((and (string=? op-str "even?") (= len 2))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-unbox-fixnum! body)
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 1)
            (wbuf-byte! body OP-I32-AND)
            (wbuf-byte! body OP-I32-EQZ)
            (emit-box-bool! body)
            #t)))

    ;; eq?
    ((and (string=? op-str "eq?") (= len 3))
     (and (codegen-expr (cadr val) body ctx)
          (codegen-expr (caddr val) body ctx)
          (begin
            (wbuf-byte! body OP-REF-EQ)
            (emit-box-bool! body)
            #t)))

    ((and (string=? op-str "eqv?") (= len 3)) (codegen-call-2! (ctx-fn-eqv ctx) val body ctx))
    ((and (string=? op-str "equal?") (= len 3)) (codegen-call-2! (ctx-fn-equal ctx) val body ctx))

    ((and (string=? op-str "apply") (= len 3)) (codegen-call-2! (ctx-fn-apply ctx) val body ctx))

    ((and (string=? op-str "number->string") (= len 2)) (codegen-call-1! (ctx-fn-num-to-str ctx) val body ctx))
    ((and (string=? op-str "string->number") (= len 2)) (codegen-call-1! (ctx-fn-str-to-num ctx) val body ctx))

    ((and (string=? op-str "display") (or (= len 2) (= len 3)))
     (codegen-io-call! (ctx-fn-display ctx) val len body ctx))

    ((and (string=? op-str "write") (or (= len 2) (= len 3)))
     (codegen-io-call! (ctx-fn-write ctx) val len body ctx))

    ;; current-error-port — P2: call get-stderr, P1: return boxed fd 2
    ((and (string=? op-str "current-error-port") (= len 1))
     (if (>= (ctx-fn-get-stderr ctx) 0)
         (begin
           (wbuf-byte! body OP-CALL) (wbuf-u32! body (ctx-fn-get-stderr ctx))
           (emit-box-fixnum! body) #t)
         (begin
           (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 2)
           (emit-box-fixnum! body) #t)))

    ;; current-output-port — P2: call get-stdout, P1: return boxed fd 1
    ((and (string=? op-str "current-output-port") (= len 1))
     (if (>= (ctx-fn-get-stdout ctx) 0)
         (begin
           (wbuf-byte! body OP-CALL) (wbuf-u32! body (ctx-fn-get-stdout ctx))
           (emit-box-fixnum! body) #t)
         (begin
           (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 1)
           (emit-box-fixnum! body) #t)))

    ;; newline
    ((and (string=? op-str "newline") (= len 1))
     (wbuf-byte! body OP-CALL)
     (wbuf-u32! body (ctx-fn-newline ctx))
     #t)

    ;; exit
    ((and (string=? op-str "exit") (= len 2))
     (and (codegen-i32-arg (cadr val) body ctx)
          (begin
            (wbuf-byte! body OP-CALL)
            (wbuf-u32! body (ctx-fn-proc-exit ctx))
            (emit-void! body) #t)))

    ;; emergency-exit (0 or 1 args)
    ((and (string=? op-str "emergency-exit") (or (= len 1) (= len 2)))
     (if (= len 1)
         (begin (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 0))
         (codegen-i32-arg (cadr val) body ctx))
     (wbuf-byte! body OP-CALL)
     (wbuf-u32! body (ctx-fn-proc-exit ctx))
     (emit-void! body) #t)

    ;; get-environment-variable
    ((and (string=? op-str "get-environment-variable") (= len 2))
     (and (codegen-expr (cadr val) body ctx)
          (begin (wbuf-byte! body OP-CALL)
                 (wbuf-u32! body (ctx-fn-get-env-var ctx)) #t)))

    ;; get-environment-variables
    ((and (string=? op-str "get-environment-variables") (= len 1))
     (wbuf-byte! body OP-CALL)
     (wbuf-u32! body (ctx-fn-get-env-vars ctx)) #t)

    ;; string-append
    ((string=? op-str "string-append")
     (let ((nargs (- len 1)))
       (cond
         ((= nargs 0)
          (wbuf-byte! body OP-GC-PREFIX)
          (wbuf-byte! body GC-ARRAY-NEW-FIXED)
          (wbuf-u32! body TY-STRING)
          (wbuf-u32! body 0)
          #t)
         ((= nargs 1)
          (codegen-expr (cadr val) body ctx))
         (else
          (let ((ok (codegen-expr (cadr val) body ctx)))
            (let loop ((i 2) (args (cddr val)))
              (when (and ok (pair? args))
                (set! ok (codegen-expr (car args) body ctx))
                (when ok
                  (wbuf-byte! body OP-CALL)
                  (wbuf-u32! body (ctx-fn-string-append ctx)))
                (loop (+ i 1) (cdr args))))
            ok)))))

    ;; command-line
    ((and (string=? op-str "command-line") (= len 1))
     (wbuf-byte! body OP-CALL)
     (wbuf-u32! body (ctx-fn-command-line ctx))
     #t)

    ;; current-milliseconds
    ((and (string=? op-str "current-milliseconds") (= len 1))
     (if (>= (ctx-fn-clock-now ctx) 0)
         (begin
           ;; monotonic-clock now() -> u64 (nanoseconds), convert to f64 ms
           (wbuf-byte! body OP-CALL)
           (wbuf-u32! body (ctx-fn-clock-now ctx))
           (wbuf-byte! body OP-F64-CONVERT-I64-S)
           (wbuf-byte! body OP-F64-CONST) (wbuf-f64! body 1000000.0)
           (wbuf-byte! body OP-F64-DIV)
           (emit-box-f64! body (ctx-ty-flonum ctx)))
         (begin
           ;; No clock available, return 0
           (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 0)
           (emit-box-fixnum! body)))
     #t)

    ((and (string=? op-str "linear-alloc") (= len 3)) (codegen-call-2! (ctx-fn-linear-alloc ctx) val body ctx))

    ;; make-bytevector (k) or (k fill)
    ((and (string=? op-str "make-bytevector") (or (= len 2) (= len 3)))
     (if (= len 2)
         ;; 1-arg: allocate zeroed
         (and (codegen-expr (cadr val) body ctx)
              (begin (wbuf-byte! body OP-CALL)
                     (wbuf-u32! body (ctx-fn-bv-alloc ctx)) #t))
         ;; 2-arg: allocate + fill
         (and (codegen-expr (cadr val) body ctx)
              (codegen-expr (caddr val) body ctx)
              (begin (wbuf-byte! body OP-CALL)
                     (wbuf-u32! body (ctx-fn-bv-alloc-fill ctx)) #t))))

    ((and (string=? op-str "bytevector-length") (= len 2)) (codegen-struct-get-i31! (ctx-ty-bytevector ctx) 1 val body ctx))

    ((and (string=? op-str "bytevector-u8-ref") (= len 3)) (codegen-bv-load! OP-I32-LOAD8-U val body ctx))
    ((and (string=? op-str "bytevector-u8-set!") (= len 4)) (codegen-bv-store-i32! OP-I32-STORE8 val body ctx))
    ((and (string=? op-str "bytevector-u32-native-ref") (= len 3)) (codegen-bv-load! OP-I32-LOAD val body ctx))
    ((and (string=? op-str "bytevector-u32-native-set!") (= len 4)) (codegen-bv-store-i32! OP-I32-STORE val body ctx))

    ;; bytevector-f64-native-set!
    ((and (string=? op-str "bytevector-f64-native-set!") (= len 4))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-cast-struct-get! body (ctx-ty-bytevector ctx) 0)
            (and (codegen-expr (caddr val) body ctx)
                 (begin
                   (emit-unbox-fixnum! body)
                   (wbuf-byte! body OP-I32-ADD)
                   (and (codegen-expr (cadddr val) body ctx)
                        (begin
                          (emit-unbox-f64! body (ctx-ty-flonum ctx))
                          (wbuf-byte! body OP-F64-STORE)
                          (wbuf-u32! body 0) (wbuf-u32! body 0)
                          (emit-void! body)
                          #t)))))))

    ((and (string=? op-str "bytevector->pointer") (= len 2)) (codegen-struct-get-i31! (ctx-ty-bytevector ctx) 0 val body ctx))

    ((and (string=? op-str "bytevector-copy-string!") (= len 4)) (codegen-call-3! (ctx-fn-bv-copy-str ctx) val body ctx))

    ;; bytevector-copy! (to at from start end)
    ((and (string=? op-str "bytevector-copy!") (= len 6))
     (let ((ok #t))
       ;; dest = to.ptr + at
       (set! ok (codegen-expr (cadr val) body ctx))
       (when ok
         (emit-cast-struct-get! body (ctx-ty-bytevector ctx) 0)
         (set! ok (codegen-i32-arg (caddr val) body ctx)))
       (when ok
         (wbuf-byte! body OP-I32-ADD)
         ;; src = from.ptr + start
         (set! ok (codegen-expr (cadddr val) body ctx)))
       (when ok
         (emit-cast-struct-get! body (ctx-ty-bytevector ctx) 0)
         (set! ok (codegen-i32-arg (list-ref val 4) body ctx)))
       (when ok
         (wbuf-byte! body OP-I32-ADD)
         ;; count = end - start
         (set! ok (codegen-i32-arg (list-ref val 5) body ctx)))
       (when ok
         (set! ok (codegen-i32-arg (list-ref val 4) body ctx)))
       (when ok
         (wbuf-byte! body OP-I32-SUB)
         (wbuf-byte! body OP-BULK-PREFIX)
         (wbuf-byte! body BULK-MEMORY-COPY)
         (wbuf-byte! body 0) (wbuf-byte! body 0)
         (emit-void! body))
       ok))

    ;; bytevector?
    ((and (string=? op-str "bytevector?") (= len 2))
     (codegen-type-pred! (ctx-ty-bytevector ctx) val body ctx))

    ;; bytevector-copy (1, 2, or 3 args)
    ((and (string=? op-str "bytevector-copy") (>= len 2) (<= len 4))
     (cond
       ((= len 2) (codegen-call-1! (ctx-fn-bv-copy ctx) val body ctx))
       ((= len 3) (codegen-call-2! (ctx-fn-bv-copy-from ctx) val body ctx))
       (else (codegen-call-3! (ctx-fn-bv-copy-range ctx) val body ctx))))

    ;; bytevector-append (variadic)
    ((string=? op-str "bytevector-append")
     (let ((nargs (- len 1)))
       (cond
         ((= nargs 0)
          (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 0)
          (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 0)
          (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-STRUCT-NEW)
          (wbuf-u32! body (ctx-ty-bytevector ctx)) #t)
         ((= nargs 1) (codegen-call-1! (ctx-fn-bv-copy ctx) val body ctx))
         (else
          (and (codegen-expr (cadr val) body ctx)
               (codegen-expr (caddr val) body ctx)
               (begin
                 (wbuf-byte! body OP-CALL)
                 (wbuf-u32! body (ctx-fn-bv-append ctx))
                 (let ((ok #t))
                   (let loop ((args (cdddr val)))
                     (when (and ok (pair? args))
                       (set! ok (codegen-expr (car args) body ctx))
                       (when ok
                         (wbuf-byte! body OP-CALL)
                         (wbuf-u32! body (ctx-fn-bv-append ctx)))
                       (loop (cdr args))))
                   ok)))))))

    ;; utf8->string (1, 2, or 3 args)
    ((and (string=? op-str "utf8->string") (>= len 2) (<= len 4))
     (cond
       ((= len 2) (codegen-call-1! (ctx-fn-utf8-to-str ctx) val body ctx))
       ((= len 3) (codegen-call-2! (ctx-fn-utf8-to-str-from ctx) val body ctx))
       (else (codegen-call-3! (ctx-fn-utf8-to-str-range ctx) val body ctx))))

    ;; string->utf8 (1, 2, or 3 args)
    ((and (string=? op-str "string->utf8") (>= len 2) (<= len 4))
     (cond
       ((= len 2) (codegen-call-1! (ctx-fn-str-to-utf8 ctx) val body ctx))
       ((= len 3) (codegen-call-2! (ctx-fn-str-to-utf8-from ctx) val body ctx))
       (else (codegen-call-3! (ctx-fn-str-to-utf8-range ctx) val body ctx))))

    ;; write-bytevector (bv port start end)
    ((and (string=? op-str "write-bytevector") (= len 5))
     (let ((ok #t))
       (if (>= (ctx-fn-stream-write ctx) 0)
           ;; P2: stream-write in 4096-byte chunks
           ;; mem[0]=ptr, mem[4]=remaining, mem[8]=handle
           (begin
             ;; Compute ptr = bv.ptr + start, store at mem[0]
             (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 0)
             (set! ok (codegen-expr (cadr val) body ctx))
             (when ok
               (emit-cast-struct-get! body (ctx-ty-bytevector ctx) 0)
               (set! ok (codegen-i32-arg (cadddr val) body ctx)))
             (when ok
               (wbuf-byte! body OP-I32-ADD)
               (wbuf-byte! body OP-I32-STORE) (wbuf-u32! body 2) (wbuf-u32! body 0)
               ;; Compute remaining = end - start, store at mem[4]
               (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 4)
               (set! ok (codegen-i32-arg (list-ref val 4) body ctx)))
             (when ok
               (set! ok (codegen-i32-arg (cadddr val) body ctx)))
             (when ok
               (wbuf-byte! body OP-I32-SUB)
               (wbuf-byte! body OP-I32-STORE) (wbuf-u32! body 2) (wbuf-u32! body 0)
               ;; Store handle at mem[8]
               (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 8)
               (set! ok (codegen-expr (caddr val) body ctx)))
             (when ok
               (emit-cast-struct-get! body (ctx-ty-port ctx) 0)
               (wbuf-byte! body OP-I32-STORE) (wbuf-u32! body 2) (wbuf-u32! body 0)
               ;; block $done
               (wbuf-byte! body OP-BLOCK) (wbuf-byte! body TYPE-VOID)
               ;; loop $again
               (wbuf-byte! body OP-LOOP) (wbuf-byte! body TYPE-VOID)
               ;; br_if $done (remaining == 0)
               (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 4)
               (wbuf-byte! body OP-I32-LOAD) (wbuf-u32! body 2) (wbuf-u32! body 0)
               (wbuf-byte! body OP-I32-EQZ)
               (wbuf-byte! body OP-BR-IF) (wbuf-u32! body 1)
               ;; chunk = min(remaining, 4096), store at mem[12]
               ;; Push addr first, then compute value
               (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 12)
               (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 4)
               (wbuf-byte! body OP-I32-LOAD) (wbuf-u32! body 2) (wbuf-u32! body 0)
               (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 4096)
               (wbuf-byte! body OP-I32-GT-S)
               (wbuf-byte! body OP-IF) (wbuf-byte! body TYPE-I32)
                 (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 4096)
               (wbuf-byte! body OP-ELSE)
                 (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 4)
                 (wbuf-byte! body OP-I32-LOAD) (wbuf-u32! body 2) (wbuf-u32! body 0)
               (wbuf-byte! body OP-END)
               ;; stack: [12, chunk] -- i32.store stores chunk at addr 12
               )
             (when ok
               (wbuf-byte! body OP-I32-STORE) (wbuf-u32! body 2) (wbuf-u32! body 0)
               ;; stream-write(handle, ptr, chunk, retptr=112)
               (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 8)
               (wbuf-byte! body OP-I32-LOAD) (wbuf-u32! body 2) (wbuf-u32! body 0) ;; handle
               (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 0)
               (wbuf-byte! body OP-I32-LOAD) (wbuf-u32! body 2) (wbuf-u32! body 0) ;; ptr
               (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 12)
               (wbuf-byte! body OP-I32-LOAD) (wbuf-u32! body 2) (wbuf-u32! body 0) ;; chunk
               (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 112) ;; retptr
               (wbuf-byte! body OP-CALL) (wbuf-u32! body (ctx-fn-stream-write ctx))
               ;; ptr += chunk
               (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 0)
               (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 0)
               (wbuf-byte! body OP-I32-LOAD) (wbuf-u32! body 2) (wbuf-u32! body 0)
               (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 12)
               (wbuf-byte! body OP-I32-LOAD) (wbuf-u32! body 2) (wbuf-u32! body 0)
               (wbuf-byte! body OP-I32-ADD)
               (wbuf-byte! body OP-I32-STORE) (wbuf-u32! body 2) (wbuf-u32! body 0)
               ;; remaining -= chunk
               (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 4)
               (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 4)
               (wbuf-byte! body OP-I32-LOAD) (wbuf-u32! body 2) (wbuf-u32! body 0)
               (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 12)
               (wbuf-byte! body OP-I32-LOAD) (wbuf-u32! body 2) (wbuf-u32! body 0)
               (wbuf-byte! body OP-I32-SUB)
               (wbuf-byte! body OP-I32-STORE) (wbuf-u32! body 2) (wbuf-u32! body 0)
               ;; br $again
               (wbuf-byte! body OP-BR) (wbuf-u32! body 0)
               ;; end loop
               (wbuf-byte! body OP-END)
               ;; end block
               (wbuf-byte! body OP-END)
               (emit-void! body)))
           ;; P1: fd_write(port.fd, iov_base=0, iov_count=1, nwritten=112)
           (begin
             ;; mem[0] = bv.ptr + start (iov_base)
             (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 0)
             (set! ok (codegen-expr (cadr val) body ctx))
             (when ok
               (emit-cast-struct-get! body (ctx-ty-bytevector ctx) 0)
               (set! ok (codegen-i32-arg (cadddr val) body ctx)))
             (when ok
               (wbuf-byte! body OP-I32-ADD)
               (wbuf-byte! body OP-I32-STORE) (wbuf-u32! body 2) (wbuf-u32! body 0)
               ;; mem[4] = end - start (iov_len)
               (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 4)
               (set! ok (codegen-i32-arg (list-ref val 4) body ctx)))
             (when ok
               (set! ok (codegen-i32-arg (cadddr val) body ctx)))
             (when ok
               (wbuf-byte! body OP-I32-SUB)
               (wbuf-byte! body OP-I32-STORE) (wbuf-u32! body 2) (wbuf-u32! body 0)
               ;; fd_write(port.fd, 0, 1, 112)
               (set! ok (codegen-expr (caddr val) body ctx)))
             (when ok
               (emit-cast-struct-get! body (ctx-ty-port ctx) 0)
               (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 0)
               (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 1)
               (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 112)
               (wbuf-byte! body OP-CALL) (wbuf-u32! body (ctx-fn-io ctx))
               (wbuf-byte! body OP-DROP)
               (emit-void! body))))
       ok))

    ((and (string=? op-str "file-exists?") (= len 2)) (codegen-call-1! (ctx-fn-file-exists ctx) val body ctx))
    ((and (string=? op-str "open-input-file") (= len 2)) (codegen-call-1! (ctx-fn-open-input-file ctx) val body ctx))
    ((and (string=? op-str "open-output-file") (= len 2)) (codegen-call-1! (ctx-fn-open-output-file ctx) val body ctx))
    ((and (string=? op-str "open-input-string") (= len 2)) (codegen-call-1! (ctx-fn-open-input-string ctx) val body ctx))
    ((and (string=? op-str "close-input-port") (= len 2)) (codegen-call-1! (ctx-fn-close-port ctx) val body ctx))
    ((and (string=? op-str "close-output-port") (= len 2)) (codegen-call-1! (ctx-fn-close-port ctx) val body ctx))
    ((and (string=? op-str "read-char") (= len 2)) (codegen-call-1! (ctx-fn-read-char-fn ctx) val body ctx))
    ((and (string=? op-str "peek-char") (= len 2)) (codegen-call-1! (ctx-fn-peek-char-fn ctx) val body ctx))

    ;; current-input-port — P2: call get-stdin, wrap in port struct; P1: return boxed fd 0
    ((and (string=? op-str "current-input-port") (= len 1))
     (if (>= (ctx-fn-get-stdin ctx) 0)
         (begin
           (wbuf-byte! body OP-CALL) (wbuf-u32! body (ctx-fn-get-stdin ctx))
           (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 0)
           (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body -1)
           (wbuf-byte! body OP-REF-NULL) (wbuf-byte! body HT-EQ)
           (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body -1)
           (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-STRUCT-NEW)
           (wbuf-u32! body (ctx-ty-port ctx)) #t)
         (begin
           (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 0)
           (emit-box-fixnum! body) #t)))

    ((and (string=? op-str "write-char") (= len 3)) (codegen-call-2! (ctx-fn-write-char-fn ctx) val body ctx))
    ((and (string=? op-str "read") (= len 2)) (codegen-call-1! (ctx-fn-read ctx) val body ctx))

    ((and (string=? op-str "char->integer") (= len 2)) (codegen-struct-get-i31! (ctx-ty-char ctx) 0 val body ctx))

    ((and (string=? op-str "char=?") (= len 3))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
            (and (codegen-expr (caddr val) body ctx)
                 (begin
                   (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
                   (wbuf-byte! body OP-I32-EQ)
                   (emit-box-bool! body) #t)))))

    ((and (string=? op-str "char<?") (= len 3))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
            (and (codegen-expr (caddr val) body ctx)
                 (begin
                   (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
                   (wbuf-byte! body OP-I32-LT-S)
                   (emit-box-bool! body) #t)))))

    ((and (string=? op-str "char>?") (= len 3))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
            (and (codegen-expr (caddr val) body ctx)
                 (begin
                   (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
                   (wbuf-byte! body OP-I32-GT-S)
                   (emit-box-bool! body) #t)))))

    ((and (string=? op-str "char<=?") (= len 3))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
            (and (codegen-expr (caddr val) body ctx)
                 (begin
                   (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
                   (wbuf-byte! body OP-I32-LE-S)
                   (emit-box-bool! body) #t)))))

    ((and (string=? op-str "char>=?") (= len 3))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
            (and (codegen-expr (caddr val) body ctx)
                 (begin
                   (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
                   (wbuf-byte! body OP-I32-GE-S)
                   (emit-box-bool! body) #t)))))

    ((and (string=? op-str "char-alphabetic?") (= len 2))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
            ;; (c | 0x20) forces uppercase to lowercase
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 32)
            (wbuf-byte! body OP-I32-OR)
            ;; unsigned(c - 97) < 26 means a-z or A-Z
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 97)
            (wbuf-byte! body OP-I32-SUB)
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 26)
            (wbuf-byte! body OP-I32-LT-U)
            (emit-box-bool! body) #t)))

    ((and (string=? op-str "char-numeric?") (= len 2))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
            ;; unsigned(c - 48) < 10 means 0-9
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 48)
            (wbuf-byte! body OP-I32-SUB)
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 10)
            (wbuf-byte! body OP-I32-LT-U)
            (emit-box-bool! body) #t)))

    ((and (string=? op-str "char-whitespace?") (= len 2))
     (and (codegen-expr (cadr val) body ctx)
          (let ((tmp-idx (ctx-nlocals ctx)))
            ;; tee the eqref to temp local
            (wbuf-byte! body OP-LOCAL-TEE) (wbuf-u32! body tmp-idx)
            ;; first extraction: unsigned(c - 9) < 5 covers tab..cr
            (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 9)
            (wbuf-byte! body OP-I32-SUB)
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 5)
            (wbuf-byte! body OP-I32-LT-U)
            ;; second extraction: c == 32 (space)
            (wbuf-byte! body OP-LOCAL-GET) (wbuf-u32! body tmp-idx)
            (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 32)
            (wbuf-byte! body OP-I32-EQ)
            (wbuf-byte! body OP-I32-OR)
            (emit-box-bool! body) #t)))

    ((and (string=? op-str "char-upcase") (= len 2))
     (and (codegen-expr (cadr val) body ctx)
          (let ((tmp-idx (ctx-nlocals ctx)))
            ;; tee the eqref to temp local
            (wbuf-byte! body OP-LOCAL-TEE) (wbuf-u32! body tmp-idx)
            ;; extract code, check if lowercase: unsigned(c - 97) < 26
            (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 97)
            (wbuf-byte! body OP-I32-SUB)
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 26)
            (wbuf-byte! body OP-I32-LT-U)
            (wbuf-byte! body OP-IF) (wbuf-byte! body HT-EQ)
            ;; lowercase: subtract 32 from code, make new char
            (wbuf-byte! body OP-LOCAL-GET) (wbuf-u32! body tmp-idx)
            (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 32)
            (wbuf-byte! body OP-I32-SUB)
            (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-STRUCT-NEW)
            (wbuf-u32! body (ctx-ty-char ctx))
            (wbuf-byte! body OP-ELSE)
            ;; not lowercase: return original char
            (wbuf-byte! body OP-LOCAL-GET) (wbuf-u32! body tmp-idx)
            (wbuf-byte! body OP-END)
            #t)))

    ((and (string=? op-str "char-downcase") (= len 2))
     (and (codegen-expr (cadr val) body ctx)
          (let ((tmp-idx (ctx-nlocals ctx)))
            ;; tee the eqref to temp local
            (wbuf-byte! body OP-LOCAL-TEE) (wbuf-u32! body tmp-idx)
            ;; extract code, check if uppercase: unsigned(c - 65) < 26
            (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 65)
            (wbuf-byte! body OP-I32-SUB)
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 26)
            (wbuf-byte! body OP-I32-LT-U)
            (wbuf-byte! body OP-IF) (wbuf-byte! body HT-EQ)
            ;; uppercase: add 32 to code, make new char
            (wbuf-byte! body OP-LOCAL-GET) (wbuf-u32! body tmp-idx)
            (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 32)
            (wbuf-byte! body OP-I32-ADD)
            (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-STRUCT-NEW)
            (wbuf-u32! body (ctx-ty-char ctx))
            (wbuf-byte! body OP-ELSE)
            ;; not uppercase: return original char
            (wbuf-byte! body OP-LOCAL-GET) (wbuf-u32! body tmp-idx)
            (wbuf-byte! body OP-END)
            #t)))

    ((and (string=? op-str "char-ci=?") (= len 3))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 32)
            (wbuf-byte! body OP-I32-OR)
            (and (codegen-expr (caddr val) body ctx)
                 (begin
                   (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
                   (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 32)
                   (wbuf-byte! body OP-I32-OR)
                   (wbuf-byte! body OP-I32-EQ)
                   (emit-box-bool! body) #t)))))

    ((and (string=? op-str "char-ci<?") (= len 3))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 32)
            (wbuf-byte! body OP-I32-OR)
            (and (codegen-expr (caddr val) body ctx)
                 (begin
                   (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
                   (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 32)
                   (wbuf-byte! body OP-I32-OR)
                   (wbuf-byte! body OP-I32-LT-S)
                   (emit-box-bool! body) #t)))))

    ((and (string=? op-str "char-ci>?") (= len 3))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 32)
            (wbuf-byte! body OP-I32-OR)
            (and (codegen-expr (caddr val) body ctx)
                 (begin
                   (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
                   (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 32)
                   (wbuf-byte! body OP-I32-OR)
                   (wbuf-byte! body OP-I32-GT-S)
                   (emit-box-bool! body) #t)))))

    ((and (string=? op-str "char-ci<=?") (= len 3))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 32)
            (wbuf-byte! body OP-I32-OR)
            (and (codegen-expr (caddr val) body ctx)
                 (begin
                   (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
                   (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 32)
                   (wbuf-byte! body OP-I32-OR)
                   (wbuf-byte! body OP-I32-LE-S)
                   (emit-box-bool! body) #t)))))

    ((and (string=? op-str "char-ci>=?") (= len 3))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 32)
            (wbuf-byte! body OP-I32-OR)
            (and (codegen-expr (caddr val) body ctx)
                 (begin
                   (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
                   (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 32)
                   (wbuf-byte! body OP-I32-OR)
                   (wbuf-byte! body OP-I32-GE-S)
                   (emit-box-bool! body) #t)))))

    ;; integer->char
    ((and (string=? op-str "integer->char") (= len 2))
     (and (codegen-i32-arg (cadr val) body ctx)
          (begin
            (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-STRUCT-NEW)
            (wbuf-u32! body (ctx-ty-char ctx)) #t)))

    ((and (string=? op-str "eof-object?") (= len 2)) (codegen-type-pred! (ctx-ty-eof ctx) val body ctx))

    ((and (string=? op-str "port?") (= len 2)) (codegen-type-pred! (ctx-ty-port ctx) val body ctx))

    ((and (string=? op-str "input-port?") (= len 2)) (codegen-port-mode-pred! 0 val body ctx))
    ((and (string=? op-str "output-port?") (= len 2)) (codegen-port-mode-pred! 1 val body ctx))

    ((and (string=? op-str "string?") (= len 2)) (codegen-type-pred! TY-STRING val body ctx))

    ((and (string=? op-str "string-length") (= len 2)) (codegen-array-len! TY-STRING val body ctx))

    ;; string-ref (s i) → char
    ((and (string=? op-str "string-ref") (= len 3))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-ref-cast! body TY-STRING)
            (and (codegen-i32-arg (caddr val) body ctx)
                 (begin
                   (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-ARRAY-GET-U)
                   (wbuf-u32! body TY-STRING)
                   (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-STRUCT-NEW)
                   (wbuf-u32! body (ctx-ty-char ctx)) #t)))))

    ;; string-set! (s i ch)
    ((and (string=? op-str "string-set!") (= len 4))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-ref-cast! body TY-STRING)
            (and (codegen-i32-arg (caddr val) body ctx)
                 (codegen-expr (cadddr val) body ctx)
                 (begin
                   (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
                   (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-ARRAY-SET)
                   (wbuf-u32! body TY-STRING)
                   (emit-void! body) #t)))))

    ;; make-string (k) or (k ch)
    ((and (string=? op-str "make-string") (or (= len 2) (= len 3)))
     (if (= len 2)
         (begin
           (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 0)
           (and (codegen-i32-arg (cadr val) body ctx)
                (begin
                  (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-ARRAY-NEW)
                  (wbuf-u32! body TY-STRING) #t)))
         (and (codegen-expr (caddr val) body ctx)
              (begin
                (emit-cast-struct-get! body (ctx-ty-char ctx) 0)
                (and (codegen-i32-arg (cadr val) body ctx)
                     (begin
                       (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-ARRAY-NEW)
                       (wbuf-u32! body TY-STRING) #t))))))

    ((and (string=? op-str "substring") (= len 4)) (codegen-call-3! (ctx-fn-substring ctx) val body ctx))

    ((and (string=? op-str "string-copy") (= len 2)) (codegen-call-1! (ctx-fn-string-copy ctx) val body ctx))
    ((and (string=? op-str "string->list") (= len 2)) (codegen-call-1! (ctx-fn-string-to-list ctx) val body ctx))
    ((and (string=? op-str "list->string") (= len 2)) (codegen-call-1! (ctx-fn-list-to-string ctx) val body ctx))
    ((and (string=? op-str "string=?") (= len 3)) (codegen-call-2! (ctx-fn-string-eq ctx) val body ctx))
    ((and (string=? op-str "string<?") (= len 3)) (codegen-call-2! (ctx-fn-string-lt ctx) val body ctx))
    ((and (string=? op-str "string-ci=?") (= len 3)) (codegen-call-2! (ctx-fn-string-ci-eq ctx) val body ctx))
    ((and (string=? op-str "string-ci<?") (= len 3)) (codegen-call-2! (ctx-fn-string-ci-lt ctx) val body ctx))
    ((and (string=? op-str "string-fill!") (= len 3)) (codegen-call-2! (ctx-fn-string-fill ctx) val body ctx))

    ((and (string=? op-str "vector?") (= len 2)) (codegen-type-pred! (ctx-ty-vector ctx) val body ctx))

    ((and (string=? op-str "vector-length") (= len 2)) (codegen-array-len! (ctx-ty-vector ctx) val body ctx))

    ((and (string=? op-str "vector-ref") (= len 3)) (codegen-array-get! (ctx-ty-vector ctx) val body ctx))
    ((and (string=? op-str "vector-set!") (= len 4)) (codegen-array-set-void! (ctx-ty-vector ctx) val body ctx))

    ;; make-vector (k) or (k fill)
    ((and (string=? op-str "make-vector") (or (= len 2) (= len 3)))
     (if (= len 2)
         ;; 1-arg: fill with void (null ref)
         (begin
           (wbuf-byte! body OP-REF-NULL) (wbuf-byte! body HT-EQ)
           (and (codegen-i32-arg (cadr val) body ctx)
                (begin
                  (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-ARRAY-NEW)
                  (wbuf-u32! body (ctx-ty-vector ctx)) #t)))
         ;; 2-arg: (make-vector k fill)
         (and (codegen-expr (caddr val) body ctx)
              (codegen-i32-arg (cadr val) body ctx)
              (begin
                (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-ARRAY-NEW)
                (wbuf-u32! body (ctx-ty-vector ctx)) #t))))

    ;; (vector e1 e2 ...) — variadic constructor
    ((and (string=? op-str "vector") (>= len 1))
     (let ((nargs (- len 1))
           (ok #t))
       (let loop ((args (cdr val)))
         (when (and ok (pair? args))
           (set! ok (codegen-expr (car args) body ctx))
           (loop (cdr args))))
       (when ok
         (wbuf-byte! body OP-GC-PREFIX)
         (wbuf-byte! body GC-ARRAY-NEW-FIXED)
         (wbuf-u32! body (ctx-ty-vector ctx))
         (wbuf-u32! body nargs))
       ok))

    ((and (string=? op-str "vector-copy") (= len 2)) (codegen-call-1! (ctx-fn-vector-copy ctx) val body ctx))
    ((and (string=? op-str "list->vector") (= len 2)) (codegen-call-1! (ctx-fn-list-to-vector ctx) val body ctx))
    ((and (string=? op-str "vector->list") (= len 2)) (codegen-call-1! (ctx-fn-vector-to-list ctx) val body ctx))
    ((and (string=? op-str "vector-fill!") (= len 3)) (codegen-call-2! (ctx-fn-vector-fill ctx) val body ctx))

    ;; binary operators (= len 3 case)
    ((and (= len 3)
          (or (string=? op-str "+") (string=? op-str "-")
              (string=? op-str "*") (string=? op-str "/")
              (string=? op-str "quotient") (string=? op-str "remainder")
              (string=? op-str "=") (string=? op-str "<")
              (string=? op-str ">") (string=? op-str "<=")
              (string=? op-str ">=")))
     (let* ((info (cond
                    ((string=? op-str "+") (cons OP-I32-ADD FL-ADD))
                    ((string=? op-str "-") (cons OP-I32-SUB FL-SUB))
                    ((string=? op-str "*") (cons OP-I32-MUL FL-MUL))
                    ((string=? op-str "/") (cons OP-I32-DIV-S FL-DIV))
                    ((string=? op-str "quotient") (cons OP-I32-DIV-S -1))
                    ((string=? op-str "remainder") (cons OP-I32-REM-S -1))
                    ((string=? op-str "=") (cons OP-I32-EQ FL-NUM-EQ))
                    ((string=? op-str "<") (cons OP-I32-LT-S FL-NUM-LT))
                    ((string=? op-str ">") (cons OP-I32-GT-S FL-NUM-GT))
                    ((string=? op-str "<=") (cons OP-I32-LE-S FL-NUM-LE))
                    ((string=? op-str ">=") (cons OP-I32-GE-S FL-NUM-GE))
                    (else (cons 0 -1))))
            (opcode (car info))
            (fl-offset (cdr info))
            (is-cmp (or (string=? op-str "=") (string=? op-str "<")
                        (string=? op-str ">") (string=? op-str "<=")
                        (string=? op-str ">="))))
       (and (codegen-expr (cadr val) body ctx)
            (if (and (>= fl-offset 0) (>= (ctx-fn-flonum-start ctx) 0))
                (and (codegen-expr (caddr val) body ctx)
                     (begin
                       (wbuf-byte! body OP-CALL)
                       (wbuf-u32! body (+ (ctx-fn-flonum-start ctx) fl-offset))
                       #t))
                (begin
                  (emit-unbox-fixnum! body)
                  (and (codegen-expr (caddr val) body ctx)
                       (begin
                         (emit-unbox-fixnum! body)
                         (wbuf-byte! body opcode)
                         (if is-cmp (emit-box-bool! body) (emit-box-fixnum! body))
                         #t)))))))

    ;; --- Low-level intrinsics (%-prefixed) ---

    ;; %mem-store8 (addr val) → store byte, push void
    ((and (string=? op-str "%mem-store8") (= len 3))
     (and (codegen-i32-arg (cadr val) body ctx)
          (codegen-i32-arg (caddr val) body ctx)
          (begin
            (wbuf-byte! body OP-I32-STORE8) (wbuf-u32! body 0) (wbuf-u32! body 0)
            (emit-void! body) #t)))

    ;; %mem-store32 (addr val) → store i32, push void
    ((and (string=? op-str "%mem-store32") (= len 3))
     (and (codegen-i32-arg (cadr val) body ctx)
          (codegen-i32-arg (caddr val) body ctx)
          (begin
            (wbuf-byte! body OP-I32-STORE) (wbuf-u32! body 2) (wbuf-u32! body 0)
            (emit-void! body) #t)))

    ;; %mem-load8 (addr) → load unsigned byte, box fixnum
    ((and (string=? op-str "%mem-load8") (= len 2))
     (and (codegen-i32-arg (cadr val) body ctx)
          (begin
            (wbuf-byte! body OP-I32-LOAD8-U) (wbuf-u32! body 0) (wbuf-u32! body 0)
            (emit-box-fixnum! body) #t)))

    ;; %mem-load32 (addr) → load i32, box fixnum
    ((and (string=? op-str "%mem-load32") (= len 2))
     (and (codegen-i32-arg (cadr val) body ctx)
          (begin
            (wbuf-byte! body OP-I32-LOAD) (wbuf-u32! body 2) (wbuf-u32! body 0)
            (emit-box-fixnum! body) #t)))

    ;; %fd-write (fd iov-ptr iov-cnt ret-ptr) → call fd_write, box result
    ((and (string=? op-str "%fd-write") (= len 5))
     (let ((ok #t))
       (set! ok (codegen-i32-arg (cadr val) body ctx))
       (when ok (set! ok (codegen-i32-arg (caddr val) body ctx)))
       (when ok (set! ok (codegen-i32-arg (cadddr val) body ctx)))
       (when ok (set! ok (codegen-i32-arg (list-ref val 4) body ctx)))
       (when ok
         (wbuf-byte! body OP-CALL)
         (wbuf-u32! body (ctx-fn-io ctx))
         (emit-box-fixnum! body))
       ok))

    ;; %stream-write (handle ptr len retptr) → call stream-write, push void
    ((and (string=? op-str "%stream-write") (= len 5))
     (let ((ok #t))
       (set! ok (codegen-i32-arg (cadr val) body ctx))
       (when ok (set! ok (codegen-i32-arg (caddr val) body ctx)))
       (when ok (set! ok (codegen-i32-arg (cadddr val) body ctx)))
       (when ok (set! ok (codegen-i32-arg (list-ref val 4) body ctx)))
       (when ok
         (wbuf-byte! body OP-CALL)
         (wbuf-u32! body (ctx-fn-stream-write ctx))
         (emit-void! body))
       ok))

    ;; %get-stdout () → call get-stdout, box result
    ((and (string=? op-str "%get-stdout") (= len 1))
     (wbuf-byte! body OP-CALL)
     (wbuf-u32! body (ctx-fn-get-stdout ctx))
     (emit-box-fixnum! body) #t)

    ;; %get-stderr () → call get-stderr, box result
    ((and (string=? op-str "%get-stderr") (= len 1))
     (wbuf-byte! body OP-CALL)
     (wbuf-u32! body (ctx-fn-get-stderr ctx))
     (emit-box-fixnum! body) #t)

    ((and (string=? op-str "%i31-add") (= len 3)) (codegen-binary-fixnum! OP-I32-ADD val body ctx))
    ((and (string=? op-str "%i31-sub") (= len 3)) (codegen-binary-fixnum! OP-I32-SUB val body ctx))
    ((and (string=? op-str "%i31-mul") (= len 3)) (codegen-binary-fixnum! OP-I32-MUL val body ctx))
    ((and (string=? op-str "%i31-div") (= len 3)) (codegen-binary-fixnum! OP-I32-DIV-S val body ctx))
    ((and (string=? op-str "%i31-rem") (= len 3)) (codegen-binary-fixnum! OP-I32-REM-S val body ctx))
    ((and (string=? op-str "%i31-rem-u") (= len 3)) (codegen-binary-fixnum! OP-I32-REM-U val body ctx))

    ;; %i31-neg (a) → 0 - a
    ((and (string=? op-str "%i31-neg") (= len 2))
     (and (begin (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 0) #t)
          (codegen-i32-arg (cadr val) body ctx)
          (begin (wbuf-byte! body OP-I32-SUB) (emit-box-fixnum! body) #t)))

    ;; %i31-eqz (a) → i32.eqz, box bool
    ((and (string=? op-str "%i31-eqz") (= len 2))
     (and (codegen-i32-arg (cadr val) body ctx)
          (begin (wbuf-byte! body OP-I32-EQZ) (emit-box-bool! body) #t)))

    ((and (string=? op-str "%i31-eq") (= len 3)) (codegen-binary-bool! OP-I32-EQ val body ctx))
    ((and (string=? op-str "%i31-lt") (= len 3)) (codegen-binary-bool! OP-I32-LT-S val body ctx))
    ((and (string=? op-str "%i31-gt") (= len 3)) (codegen-binary-bool! OP-I32-GT-S val body ctx))
    ((and (string=? op-str "%i31-le") (= len 3)) (codegen-binary-bool! OP-I32-LE-S val body ctx))
    ((and (string=? op-str "%i31-ge") (= len 3)) (codegen-binary-bool! OP-I32-GE-S val body ctx))
    ((and (string=? op-str "%i31-and") (= len 3)) (codegen-binary-fixnum! OP-I32-AND val body ctx))
    ((and (string=? op-str "%i31-xor") (= len 3)) (codegen-binary-fixnum! OP-I32-XOR val body ctx))
    ((and (string=? op-str "%i31-or") (= len 3)) (codegen-binary-fixnum! OP-I32-OR val body ctx))
    ((and (string=? op-str "%i31-shl") (= len 3)) (codegen-binary-fixnum! OP-I32-SHL val body ctx))
    ((and (string=? op-str "bitwise-and") (= len 3)) (codegen-binary-fixnum! OP-I32-AND val body ctx))
    ((and (string=? op-str "bitwise-ior") (= len 3)) (codegen-binary-fixnum! OP-I32-OR val body ctx))

    ;; arithmetic-shift (value amount)
    ;; Positive amount = left shift, negative = right shift
    ;; Only constant shift amounts are supported for now
    ((and (string=? op-str "arithmetic-shift") (= len 3))
     (let ((shift-amt (caddr val)))
       (cond
        ((and (integer? shift-amt) (exact? shift-amt) (>= shift-amt 0))
         (and (codegen-i32-arg (cadr val) body ctx)
              (codegen-i32-arg shift-amt body ctx)
              (begin (wbuf-byte! body OP-I32-SHL) (emit-box-fixnum! body) #t)))
        ((and (integer? shift-amt) (exact? shift-amt) (< shift-amt 0))
         (and (codegen-i32-arg (cadr val) body ctx)
              (codegen-i32-arg (- 0 shift-amt) body ctx)
              (begin (wbuf-byte! body OP-I32-SHR-S) (emit-box-fixnum! body) #t)))
        (else
         (error "arithmetic-shift: variable shift amounts not supported")))))

    ((and (string=? op-str "%i31-div-u") (= len 3)) (codegen-binary-fixnum! OP-I32-DIV-U val body ctx))
    ((and (string=? op-str "%i31-ge-u") (= len 3)) (codegen-binary-bool! OP-I32-GE-U val body ctx))
    ((and (string=? op-str "%i31-ne") (= len 3)) (codegen-binary-bool! OP-I32-NE val body ctx))

    ((and (string=? op-str "%i31?") (= len 2)) (codegen-type-pred! HT-I31 val body ctx))
    ((and (string=? op-str "%flonum?") (= len 2)) (codegen-type-pred! (ctx-ty-flonum ctx) val body ctx))
    ((and (string=? op-str "%rational?") (= len 2)) (codegen-type-pred! (ctx-ty-rational ctx) val body ctx))
    ((and (string=? op-str "%complex?") (= len 2)) (codegen-type-pred! (ctx-ty-complex ctx) val body ctx))
    ((and (string=? op-str "char?") (= len 2)) (codegen-type-pred! (ctx-ty-char ctx) val body ctx))
    ((and (string=? op-str "%char?") (= len 2)) (codegen-type-pred! (ctx-ty-char ctx) val body ctx))
    ((and (string=? op-str "%string?") (= len 2)) (codegen-type-pred! TY-STRING val body ctx))
    ((and (string=? op-str "%symbol?") (= len 2)) (codegen-type-pred! (ctx-ty-symbol ctx) val body ctx))
    ((and (string=? op-str "%pair?") (= len 2)) (codegen-type-pred! TY-PAIR val body ctx))

    ((and (string=? op-str "%car") (= len 2)) (codegen-struct-get! TY-PAIR 0 val body ctx))
    ((and (string=? op-str "%cdr") (= len 2)) (codegen-struct-get! TY-PAIR 1 val body ctx))

    ;; %ref-eq (a b) → ref.eq, box bool
    ((and (string=? op-str "%ref-eq") (= len 3))
     (and (codegen-expr (cadr val) body ctx)
          (codegen-expr (caddr val) body ctx)
          (begin (wbuf-byte! body OP-REF-EQ) (emit-box-bool! body) #t)))

    ;; %ref-null → ref.null eq
    ((and (string=? op-str "%ref-null") (= len 1))
     (wbuf-byte! body OP-REF-NULL) (wbuf-byte! body HT-EQ) #t)

    ;; %ref-is-null (x) → ref.is_null, box bool
    ((and (string=? op-str "%ref-is-null") (= len 2))
     (and (codegen-expr (cadr val) body ctx)
          (begin (wbuf-byte! body OP-REF-IS-NULL) (emit-box-bool! body) #t)))

    ((and (string=? op-str "%f64-add") (= len 3)) (codegen-binary-f64! OP-F64-ADD val body ctx))
    ((and (string=? op-str "%f64-sub") (= len 3)) (codegen-binary-f64! OP-F64-SUB val body ctx))
    ((and (string=? op-str "%f64-mul") (= len 3)) (codegen-binary-f64! OP-F64-MUL val body ctx))
    ((and (string=? op-str "%f64-div") (= len 3)) (codegen-binary-f64! OP-F64-DIV val body ctx))

    ((and (string=? op-str "%f64-neg") (= len 2)) (codegen-unary-f64! OP-F64-NEG val body ctx))
    ((and (string=? op-str "%f64-abs") (= len 2)) (codegen-unary-f64! OP-F64-ABS val body ctx))
    ((and (string=? op-str "%f64-sqrt") (= len 2)) (codegen-unary-f64! OP-F64-SQRT val body ctx))
    ((and (string=? op-str "%f64-floor") (= len 2)) (codegen-unary-f64! OP-F64-FLOOR val body ctx))
    ((and (string=? op-str "%f64-ceil") (= len 2)) (codegen-unary-f64! OP-F64-CEIL val body ctx))
    ((and (string=? op-str "%f64-trunc") (= len 2)) (codegen-unary-f64! OP-F64-TRUNC val body ctx))
    ((and (string=? op-str "%f64-nearest") (= len 2)) (codegen-unary-f64! OP-F64-NEAREST val body ctx))
    ((and (string=? op-str "%f64-copysign") (= len 3)) (codegen-binary-f64! OP-F64-COPYSIGN val body ctx))

    ((and (string=? op-str "%f64-eq") (= len 3)) (codegen-f64-cmp! OP-F64-EQ val body ctx))
    ((and (string=? op-str "%f64-ne") (= len 3)) (codegen-f64-cmp! OP-F64-NE val body ctx))
    ((and (string=? op-str "%f64-lt") (= len 3)) (codegen-f64-cmp! OP-F64-LT val body ctx))
    ((and (string=? op-str "%f64-gt") (= len 3)) (codegen-f64-cmp! OP-F64-GT val body ctx))
    ((and (string=? op-str "%f64-le") (= len 3)) (codegen-f64-cmp! OP-F64-LE val body ctx))
    ((and (string=? op-str "%f64-ge") (= len 3)) (codegen-f64-cmp! OP-F64-GE val body ctx))

    ;; %f64-convert-i31 (x) → unbox i31, f64.convert_i32_s, box flonum
    ((and (string=? op-str "%f64-convert-i31") (= len 2))
     (and (codegen-i32-arg (cadr val) body ctx)
          (begin
            (wbuf-byte! body OP-F64-CONVERT-I32-S)
            (emit-box-f64! body (ctx-ty-flonum ctx)) #t)))

    ;; %f64-trunc-to-i31 (x) → unbox flonum, i32.trunc_f64_s, box fixnum
    ((and (string=? op-str "%f64-trunc-to-i31") (= len 2))
     (and (codegen-f64-arg (cadr val) body ctx)
          (begin
            (wbuf-byte! body OP-I32-TRUNC-F64-S)
            (emit-box-fixnum! body) #t)))

    ((and (string=? op-str "%rational-num") (= len 2)) (codegen-struct-get-i31! (ctx-ty-rational ctx) 0 val body ctx))
    ((and (string=? op-str "%rational-den") (= len 2)) (codegen-struct-get-i31! (ctx-ty-rational ctx) 1 val body ctx))

    ;; %make-rational (n d) → unbox both, struct.new
    ((and (string=? op-str "%make-rational") (= len 3))
     (and (codegen-i32-arg (cadr val) body ctx)
          (codegen-i32-arg (caddr val) body ctx)
          (begin
            (emit-box-rational! body (ctx-ty-rational ctx)) #t)))

    ((and (string=? op-str "%complex-real") (= len 2)) (codegen-struct-get! (ctx-ty-complex ctx) 0 val body ctx))
    ((and (string=? op-str "%complex-imag") (= len 2)) (codegen-struct-get! (ctx-ty-complex ctx) 1 val body ctx))

    ;; %make-complex-raw (real imag) → struct.new (both eqref)
    ((and (string=? op-str "%make-complex-raw") (= len 3))
     (and (codegen-expr (cadr val) body ctx)
          (codegen-expr (caddr val) body ctx)
          (begin
            (emit-box-complex! body (ctx-ty-complex ctx)) #t)))

    ((and (string=? op-str "%char-code") (= len 2)) (codegen-struct-get-i31! (ctx-ty-char ctx) 0 val body ctx))

    ;; %make-char (code) → unbox, struct.new $char
    ((and (string=? op-str "%make-char") (= len 2))
     (and (codegen-i32-arg (cadr val) body ctx)
          (begin
            (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-STRUCT-NEW)
            (wbuf-u32! body (ctx-ty-char ctx)) #t)))

    ((and (string=? op-str "%symbol-string") (= len 2)) (codegen-struct-get! (ctx-ty-symbol ctx) 0 val body ctx))

    ((and (string=? op-str "%string-length") (= len 2)) (codegen-array-len! TY-STRING val body ctx))

    ((and (string=? op-str "%string-ref") (= len 3))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-ref-cast! body TY-STRING)
            (and (codegen-i32-arg (caddr val) body ctx)
                 (begin
                   (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-ARRAY-GET-U)
                   (wbuf-u32! body TY-STRING)
                   (emit-box-fixnum! body) #t)))))

    ((and (string=? op-str "%string-set!") (= len 4))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-ref-cast! body TY-STRING)
            (and (codegen-i32-arg (caddr val) body ctx)
                 (codegen-i32-arg (cadddr val) body ctx)
                 (begin
                   (wbuf-byte! body OP-GC-PREFIX)
                   (wbuf-byte! body GC-ARRAY-SET)
                   (wbuf-u32! body TY-STRING)
                   (emit-void! body) #t)))))

    ;; %make-string (len init) → unbox both, array.new
    ((and (string=? op-str "%make-string") (= len 3))
     (and (codegen-i32-arg (caddr val) body ctx)
          (codegen-i32-arg (cadr val) body ctx)
          (begin
            (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-ARRAY-NEW)
            (wbuf-u32! body TY-STRING) #t)))

    ((and (string=? op-str "%vector-length") (= len 2)) (codegen-array-len! (ctx-ty-vector ctx) val body ctx))
    ((and (string=? op-str "%vector-ref") (= len 3)) (codegen-array-get! (ctx-ty-vector ctx) val body ctx))
    ((and (string=? op-str "%vector-set!") (= len 4)) (codegen-array-set-void! (ctx-ty-vector ctx) val body ctx))

    ;; %make-vector (init len) → init is eqref, len is i31
    ((and (string=? op-str "%make-vector") (= len 3))
     (and (codegen-expr (cadr val) body ctx)
          (codegen-i32-arg (caddr val) body ctx)
          (begin
            (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-ARRAY-NEW)
            (wbuf-u32! body (ctx-ty-vector ctx)) #t)))

    ;; %memory-size → memory.size, shift left 16, box fixnum
    ((and (string=? op-str "%memory-size") (= len 1))
     (wbuf-byte! body OP-MEMORY-SIZE) (wbuf-u32! body 0)
     (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 16)
     (wbuf-byte! body OP-I32-SHL)
     (emit-box-fixnum! body) #t)

    ;; %memory-grow (n) → unbox, memory.grow, box fixnum
    ((and (string=? op-str "%memory-grow") (= len 2))
     (and (codegen-i32-arg (cadr val) body ctx)
          (begin
            (wbuf-byte! body OP-MEMORY-GROW) (wbuf-u32! body 0)
            (emit-box-fixnum! body) #t)))

    ;; %memory-fill (dest val count) → memory.fill, push void
    ((and (string=? op-str "%memory-fill") (= len 4))
     (and (codegen-i32-arg (cadr val) body ctx)
          (codegen-i32-arg (caddr val) body ctx)
          (codegen-i32-arg (cadddr val) body ctx)
          (begin
            (wbuf-byte! body OP-BULK-PREFIX)
            (wbuf-byte! body BULK-MEMORY-FILL)
            (wbuf-byte! body 0)
            (emit-void! body) #t)))

    ;; %memory-copy (dest src count) → memory.copy, push void
    ((and (string=? op-str "%memory-copy") (= len 4))
     (and (codegen-i32-arg (cadr val) body ctx)
          (codegen-i32-arg (caddr val) body ctx)
          (codegen-i32-arg (cadddr val) body ctx)
          (begin
            (wbuf-byte! body OP-BULK-PREFIX)
            (wbuf-byte! body BULK-MEMORY-COPY)
            (wbuf-byte! body 0)
            (wbuf-byte! body 0)
            (emit-void! body) #t)))

    ;; %global-get-bump-ptr → global.get, box fixnum
    ((and (string=? op-str "%global-get-bump-ptr") (= len 1))
     (wbuf-byte! body OP-GLOBAL-GET) (wbuf-u32! body (ctx-bump-ptr-idx ctx))
     (emit-box-fixnum! body) #t)

    ;; %global-set-bump-ptr! (v) → unbox, global.set, push void
    ((and (string=? op-str "%global-set-bump-ptr!") (= len 2))
     (and (codegen-i32-arg (cadr val) body ctx)
          (begin
            (wbuf-byte! body OP-GLOBAL-SET) (wbuf-u32! body (ctx-bump-ptr-idx ctx))
            (emit-void! body) #t)))

    ;; %wasi-args-sizes-get (a b) → unbox both, call, box result
    ((and (string=? op-str "%wasi-args-sizes-get") (= len 3))
     (and (codegen-i32-arg (cadr val) body ctx)
          (codegen-i32-arg (caddr val) body ctx)
          (begin
            (wbuf-byte! body OP-CALL)
            (wbuf-u32! body (ctx-fn-args-sizes-get ctx))
            (emit-box-fixnum! body) #t)))

    ;; %wasi-args-get (a b) → unbox both, call, box result
    ((and (string=? op-str "%wasi-args-get") (= len 3))
     (and (codegen-i32-arg (cadr val) body ctx)
          (codegen-i32-arg (caddr val) body ctx)
          (begin
            (wbuf-byte! body OP-CALL)
            (wbuf-u32! body (ctx-fn-args-get ctx))
            (emit-box-fixnum! body) #t)))

    ;; %call-get-arguments (retptr) → call P2 get-arguments import
    ((and (string=? op-str "%call-get-arguments") (= len 2))
     (and (codegen-i32-arg (cadr val) body ctx)
          (begin
            (wbuf-byte! body OP-CALL)
            (wbuf-u32! body (ctx-fn-get-arguments ctx))
            (emit-void! body) #t)))

    ;; %call-get-environment (retptr) → call P2 get-environment import
    ((and (string=? op-str "%call-get-environment") (= len 2))
     (and (codegen-i32-arg (cadr val) body ctx)
          (begin
            (wbuf-byte! body OP-CALL)
            (wbuf-u32! body (ctx-fn-get-environment ctx))
            (emit-void! body) #t)))

    ;; %call-get-stdin () → call P2 get-stdin, box result as fixnum
    ((and (string=? op-str "%call-get-stdin") (= len 1))
     (wbuf-byte! body OP-CALL)
     (wbuf-u32! body (ctx-fn-get-stdin ctx))
     (emit-box-fixnum! body) #t)

    ;; %call-stream-read (handle len_i32 retptr) → extend len to i64, call blocking-read
    ((and (string=? op-str "%call-stream-read") (= len 4))
     (let ((ok #t))
       (set! ok (codegen-i32-arg (cadr val) body ctx))
       (when ok (set! ok (codegen-i32-arg (caddr val) body ctx)))
       (when ok (wbuf-byte! body OP-I64-EXTEND-I32-U))
       (when ok (set! ok (codegen-i32-arg (cadddr val) body ctx)))
       (when ok
         (wbuf-byte! body OP-CALL)
         (wbuf-u32! body (ctx-fn-stream-read ctx))
         (emit-void! body))
       ok))

    ;; %call-get-directories (retptr) → call P2 get-directories
    ((and (string=? op-str "%call-get-directories") (= len 2))
     (and (codegen-i32-arg (cadr val) body ctx)
          (begin
            (wbuf-byte! body OP-CALL)
            (wbuf-u32! body (ctx-fn-get-directories ctx))
            (emit-void! body) #t)))

    ;; %call-open-at (dirfd path-flags path-ptr path-len open-flags desc-flags retptr) → 7 i32 args
    ((and (string=? op-str "%call-open-at") (= len 8))
     (let ((ok #t))
       (set! ok (codegen-i32-arg (cadr val) body ctx))
       (when ok (set! ok (codegen-i32-arg (caddr val) body ctx)))
       (when ok (set! ok (codegen-i32-arg (cadddr val) body ctx)))
       (when ok (set! ok (codegen-i32-arg (list-ref val 4) body ctx)))
       (when ok (set! ok (codegen-i32-arg (list-ref val 5) body ctx)))
       (when ok (set! ok (codegen-i32-arg (list-ref val 6) body ctx)))
       (when ok (set! ok (codegen-i32-arg (list-ref val 7) body ctx)))
       (when ok
         (wbuf-byte! body OP-CALL)
         (wbuf-u32! body (ctx-fn-open-at ctx))
         (emit-void! body))
       ok))

    ;; %call-read-via-stream (handle offset_i32 retptr) → extend offset to i64, call
    ((and (string=? op-str "%call-read-via-stream") (= len 4))
     (let ((ok #t))
       (set! ok (codegen-i32-arg (cadr val) body ctx))
       (when ok (set! ok (codegen-i32-arg (caddr val) body ctx)))
       (when ok (wbuf-byte! body OP-I64-EXTEND-I32-U))
       (when ok (set! ok (codegen-i32-arg (cadddr val) body ctx)))
       (when ok
         (wbuf-byte! body OP-CALL)
         (wbuf-u32! body (ctx-fn-read-via-stream ctx))
         (emit-void! body))
       ok))

    ;; %call-write-via-stream (handle offset_i32 retptr) → extend offset to i64, call
    ((and (string=? op-str "%call-write-via-stream") (= len 4))
     (let ((ok #t))
       (set! ok (codegen-i32-arg (cadr val) body ctx))
       (when ok (set! ok (codegen-i32-arg (caddr val) body ctx)))
       (when ok (wbuf-byte! body OP-I64-EXTEND-I32-U))
       (when ok (set! ok (codegen-i32-arg (cadddr val) body ctx)))
       (when ok
         (wbuf-byte! body OP-CALL)
         (wbuf-u32! body (ctx-fn-write-via-stream ctx))
         (emit-void! body))
       ok))

    ;; %call-drop-descriptor (handle) → call resource.drop
    ((and (string=? op-str "%call-drop-descriptor") (= len 2))
     (and (codegen-i32-arg (cadr val) body ctx)
          (begin
            (wbuf-byte! body OP-CALL)
            (wbuf-u32! body (ctx-fn-drop-descriptor ctx))
            (emit-void! body) #t)))

    ;; %call-drop-input-stream (handle) → call resource.drop
    ((and (string=? op-str "%call-drop-input-stream") (= len 2))
     (and (codegen-i32-arg (cadr val) body ctx)
          (begin
            (wbuf-byte! body OP-CALL)
            (wbuf-u32! body (ctx-fn-drop-input-stream ctx))
            (emit-void! body) #t)))

    ;; %call-drop-output-stream (handle) → call resource.drop
    ((and (string=? op-str "%call-drop-output-stream") (= len 2))
     (and (codegen-i32-arg (cadr val) body ctx)
          (begin
            (wbuf-byte! body OP-CALL)
            (wbuf-u32! body (ctx-fn-drop-output-stream ctx))
            (emit-void! body) #t)))

    ;; %wasi-environ-sizes-get (a b)
    ((and (string=? op-str "%wasi-environ-sizes-get") (= len 3))
     (and (codegen-i32-arg (cadr val) body ctx)
          (codegen-i32-arg (caddr val) body ctx)
          (begin
            (wbuf-byte! body OP-CALL)
            (wbuf-u32! body (ctx-fn-environ-sizes-get ctx))
            (emit-box-fixnum! body) #t)))

    ;; %wasi-environ-get (a b)
    ((and (string=? op-str "%wasi-environ-get") (= len 3))
     (and (codegen-i32-arg (cadr val) body ctx)
          (codegen-i32-arg (caddr val) body ctx)
          (begin
            (wbuf-byte! body OP-CALL)
            (wbuf-u32! body (ctx-fn-environ-get ctx))
            (emit-box-fixnum! body) #t)))

    ;; %wasi-fd-read (fd iovs iovcnt nread_ptr) → call fd_read, box result
    ((and (string=? op-str "%wasi-fd-read") (= len 5))
     (let ((ok #t))
       (set! ok (codegen-i32-arg (cadr val) body ctx))
       (when ok (set! ok (codegen-i32-arg (caddr val) body ctx)))
       (when ok (set! ok (codegen-i32-arg (cadddr val) body ctx)))
       (when ok (set! ok (codegen-i32-arg (list-ref val 4) body ctx)))
       (when ok
         (wbuf-byte! body OP-CALL)
         (wbuf-u32! body (ctx-fn-fd-read ctx))
         (emit-box-fixnum! body))
       ok))

    ;; %wasi-fd-close (fd) → call fd_close, box result
    ((and (string=? op-str "%wasi-fd-close") (= len 2))
     (and (codegen-i32-arg (cadr val) body ctx)
          (begin
            (wbuf-byte! body OP-CALL)
            (wbuf-u32! body (ctx-fn-fd-close ctx))
            (emit-box-fixnum! body) #t)))

    ;; %wasi-path-open (dirfd dirflags path_ptr path_len oflags rights fdflags result_ptr)
    ;; → unbox all to i32, extend rights to i64, insert rights_inheriting=0, call path_open, box
    ((and (string=? op-str "%wasi-path-open") (= len 9))
     (let ((ok #t))
       (set! ok (codegen-i32-arg (cadr val) body ctx))          ;; dirfd
       (when ok (set! ok (codegen-i32-arg (caddr val) body ctx)))      ;; dirflags
       (when ok (set! ok (codegen-i32-arg (cadddr val) body ctx)))     ;; path_ptr
       (when ok (set! ok (codegen-i32-arg (list-ref val 4) body ctx))) ;; path_len
       (when ok (set! ok (codegen-i32-arg (list-ref val 5) body ctx))) ;; oflags
       (when ok (set! ok (codegen-i32-arg (list-ref val 6) body ctx))) ;; rights (i32)
       (when ok
         (wbuf-byte! body OP-I64-EXTEND-I32-U)                        ;; rights → i64
         (wbuf-byte! body OP-I64-CONST) (wbuf-i64! body 0))           ;; rights_inheriting = 0
       (when ok (set! ok (codegen-i32-arg (list-ref val 7) body ctx))) ;; fdflags
       (when ok (set! ok (codegen-i32-arg (list-ref val 8) body ctx))) ;; result_ptr
       (when ok
         (wbuf-byte! body OP-CALL)
         (wbuf-u32! body (ctx-fn-path-open ctx))
         (emit-box-fixnum! body))
       ok))

    ;; %make-port (fd mode buf src-str pos) → i32 i32 i32 ref i32, struct.new $ty_port
    ((and (string=? op-str "%make-port") (= len 6))
     (and (codegen-i32-arg (cadr val) body ctx)
          (codegen-i32-arg (caddr val) body ctx)
          (codegen-i32-arg (cadddr val) body ctx)
          (codegen-expr (list-ref val 4) body ctx)
          (codegen-i32-arg (list-ref val 5) body ctx)
          (begin
            (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-STRUCT-NEW)
            (wbuf-u32! body (ctx-ty-port ctx)) #t)))

    ((and (string=? op-str "%port-fd") (= len 2)) (codegen-struct-get-i31! (ctx-ty-port ctx) 0 val body ctx))
    ((and (string=? op-str "%port-mode") (= len 2)) (codegen-struct-get-i31! (ctx-ty-port ctx) 1 val body ctx))
    ((and (string=? op-str "%port-buf") (= len 2)) (codegen-struct-get-i31! (ctx-ty-port ctx) 2 val body ctx))

    ;; %port-str — returns the ref field (field 3), no i31 boxing
    ((and (string=? op-str "%port-str") (= len 2)) (codegen-struct-get! (ctx-ty-port ctx) 3 val body ctx))

    ((and (string=? op-str "%port-pos") (= len 2)) (codegen-struct-get-i31! (ctx-ty-port ctx) 4 val body ctx))

    ((and (string=? op-str "%port-set-buf!") (= len 3))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-ref-cast! body (ctx-ty-port ctx))
            (and (codegen-i32-arg (caddr val) body ctx)
                 (begin
                   (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-STRUCT-SET)
                   (wbuf-u32! body (ctx-ty-port ctx)) (wbuf-u32! body 2)
                   (emit-void! body) #t)))))

    ((and (string=? op-str "%port-set-pos!") (= len 3))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-ref-cast! body (ctx-ty-port ctx))
            (and (codegen-i32-arg (caddr val) body ctx)
                 (begin
                   (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-STRUCT-SET)
                   (wbuf-u32! body (ctx-ty-port ctx)) (wbuf-u32! body 4)
                   (emit-void! body) #t)))))

    ((and (string=? op-str "%port?") (= len 2)) (codegen-type-pred! (ctx-ty-port ctx) val body ctx))

    ;; %eof-new () → struct.new $ty_eof
    ((and (string=? op-str "%eof-new") (= len 1))
     (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-STRUCT-NEW)
     (wbuf-u32! body (ctx-ty-eof ctx)) #t)

    ((and (string=? op-str "%eof?") (= len 2)) (codegen-type-pred! (ctx-ty-eof ctx) val body ctx))

    ;; %unbox-f64 (x) → extract f64, box as flonum
    ;; (identity for flonums, used to get f64 from eqref containing flonum)
    ((and (string=? op-str "%unbox-f64") (= len 2))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-unbox-f64! body (ctx-ty-flonum ctx))
            (emit-box-f64! body (ctx-ty-flonum ctx)) #t)))

    ;; %box-f64 (x) → used when we have an eqref that's a flonum, returns as-is
    ;; Actually, this is for creating flonum from f64-producing intrinsics - see %f64-*

    ;; %f64-ldexp (k) → construct 2^k as f64 via f64.reinterpret(i64((1023+k) << 52))
    ((and (string=? op-str "%f64-ldexp") (= len 2))
     (and (codegen-i32-arg (cadr val) body ctx)
          (begin
            (wbuf-byte! body OP-I64-EXTEND-I32-S)
            (wbuf-byte! body OP-I64-CONST) (wbuf-i64! body 1023)
            (wbuf-byte! body OP-I64-ADD)
            (wbuf-byte! body OP-I64-CONST) (wbuf-i64! body 52)
            (wbuf-byte! body OP-I64-SHL)
            (wbuf-byte! body OP-F64-REINTERPRET-I64)
            (emit-box-f64! body (ctx-ty-flonum ctx)) #t)))

    ;; %f64-exponent (x) → extract unbiased exponent from f64
    ((and (string=? op-str "%f64-exponent") (= len 2))
     (and (codegen-f64-arg (cadr val) body ctx)
          (begin
            (wbuf-byte! body OP-I64-REINTERPRET-F64)
            (wbuf-byte! body OP-I64-CONST) (wbuf-i64! body 52)
            (wbuf-byte! body OP-I64-SHR-U)
            (wbuf-byte! body OP-I32-WRAP-I64)
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 2047)
            (wbuf-byte! body OP-I32-AND)
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 1023)
            (wbuf-byte! body OP-I32-SUB)
            (emit-box-fixnum! body) #t)))

    ;; %f64-mantissa (x) → extract mantissa normalized to [1,2)
    ((and (string=? op-str "%f64-mantissa") (= len 2))
     (and (codegen-f64-arg (cadr val) body ctx)
          (begin
            (wbuf-byte! body OP-I64-REINTERPRET-F64)
            ;; clear exponent bits, set exponent to 1023 (=1.0 * mantissa)
            ;; 0xFFFFFFFFFFFFF (52-bit mask) — LEB128 bytes emitted directly
            ;; to avoid bignum literals that don't fit in i31ref
            (wbuf-byte! body OP-I64-CONST)
            (wbuf-bytes-list! body '(#xff #xff #xff #xff #xff #xff #xff #x07))
            (wbuf-byte! body OP-I64-AND)
            ;; 0x3FF0000000000000 (exp=1023) — LEB128 bytes emitted directly
            (wbuf-byte! body OP-I64-CONST)
            (wbuf-bytes-list! body '(#x80 #x80 #x80 #x80 #x80 #x80 #x80 #xf8 #x3f))
            (wbuf-byte! body OP-I64-OR)
            (wbuf-byte! body OP-F64-REINTERPRET-I64)
            (emit-box-f64! body (ctx-ty-flonum ctx)) #t)))

    ;; %make-flonum (f64-expr) → box f64 as flonum struct
    ;; Used when we need to box a raw f64 that's on the WASM stack
    ((and (string=? op-str "%make-flonum") (= len 2))
     (and (codegen-f64-arg (cadr val) body ctx)
          (begin (emit-box-f64! body (ctx-ty-flonum ctx)) #t)))

    ;; %flonum-value (x) → unbox flonum to f64, rebox
    ;; Actually returns the f64 value boxed in a new flonum
    ;; This is identity on flonums, used for type clarity

    ;; %make-bytevector-struct (ptr len) → struct.new $bytevector
    ((and (string=? op-str "%make-bytevector-struct") (= len 3))
     (and (codegen-i32-arg (cadr val) body ctx)
          (codegen-i32-arg (caddr val) body ctx)
          (begin
            (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-STRUCT-NEW)
            (wbuf-u32! body (ctx-ty-bytevector ctx)) #t)))

    ((and (string=? op-str "%bytevector-ptr") (= len 2)) (codegen-struct-get-i31! (ctx-ty-bytevector ctx) 0 val body ctx))

    ;; %block-void (body ...) → block with void type
    ((and (string=? op-str "%block-void") (>= len 2))
     (wbuf-byte! body OP-BLOCK) (wbuf-byte! body TYPE-VOID)
     (let ((ok (codegen-body (cdr val) body ctx)))
       (when ok
         (wbuf-byte! body OP-DROP)
         (wbuf-byte! body OP-END)
         (emit-void! body))
       ok))

    ;; %block-eqref (body ...) → block with eqref type
    ((and (string=? op-str "%block-eqref") (>= len 2))
     (wbuf-byte! body OP-BLOCK) (wbuf-byte! body HT-EQ)
     (let ((ok (codegen-body (cdr val) body ctx)))
       (when ok (wbuf-byte! body OP-END))
       ok))

    ;; %loop-void (body ...) → loop with void type
    ((and (string=? op-str "%loop-void") (>= len 2))
     (wbuf-byte! body OP-LOOP) (wbuf-byte! body TYPE-VOID)
     (let ((ok (codegen-body (cdr val) body ctx)))
       (when ok
         (wbuf-byte! body OP-DROP)
         (wbuf-byte! body OP-END)
         (emit-void! body))
       ok))

    ;; %br (depth) → unconditional branch
    ((and (string=? op-str "%br") (= len 2))
     (let ((depth (cadr val)))
       (wbuf-byte! body OP-BR) (wbuf-u32! body depth)
       (emit-void! body) #t))

    ;; %br-if (depth cond) → conditional branch
    ((and (string=? op-str "%br-if") (= len 3))
     (and (codegen-i32-arg (caddr val) body ctx)
          (begin
            (wbuf-byte! body OP-BR-IF) (wbuf-u32! body (cadr val))
            (emit-void! body) #t)))

    ;; %local-set! (idx val) → compile val, unbox i31, local.set
    ((and (string=? op-str "%local-set!") (= len 3))
     (and (codegen-expr (caddr val) body ctx)
          (begin
            (wbuf-byte! body OP-LOCAL-SET) (wbuf-u32! body (cadr val))
            (emit-void! body) #t)))

    ;; %local-get (idx) → local.get
    ((and (string=? op-str "%local-get") (= len 2))
     (wbuf-byte! body OP-LOCAL-GET) (wbuf-u32! body (cadr val)) #t)

    ;; %local-tee (idx val) → compile val, local.tee
    ((and (string=? op-str "%local-tee") (= len 3))
     (and (codegen-expr (caddr val) body ctx)
          (begin
            (wbuf-byte! body OP-LOCAL-TEE) (wbuf-u32! body (cadr val)) #t)))

    ;; %unreachable → emit unreachable trap
    ((and (string=? op-str "%unreachable") (= len 1))
     (wbuf-byte! body #x00)
     (emit-void! body) #t)

    ;; %drop (expr) → compile, drop, push void
    ((and (string=? op-str "%drop") (= len 2))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (wbuf-byte! body OP-DROP)
            (emit-void! body) #t)))

    ;; %if-i32 (cond then else) → compile cond as i32, if
    ;; Used for control flow based on unboxed i32 conditions
    ((and (string=? op-str "%if-i32") (or (= len 3) (= len 4)))
     (and (codegen-i32-arg (cadr val) body ctx)
          (begin
            (wbuf-byte! body OP-IF) (wbuf-byte! body HT-EQ)
            (and (codegen-expr/tail (caddr val) body ctx tail?)
                 (begin
                   (wbuf-byte! body OP-ELSE)
                   (if (= len 4)
                       (codegen-expr/tail (cadddr val) body ctx tail?)
                       (begin (emit-void! body) #t))
                   (wbuf-byte! body OP-END) #t)))))

    ;; %make-promise (thunk) → struct.new $promise with state=0
    ((and (string=? op-str "%make-promise") (= len 2))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 0)
            (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-STRUCT-NEW)
            (wbuf-u32! body (ctx-ty-promise ctx)) #t)))

    ;; %promise-ref (p) → get field 0 (value/thunk)
    ((and (string=? op-str "%promise-ref") (= len 2))
     (codegen-struct-get! (ctx-ty-promise ctx) 0 val body ctx))

    ;; %promise-state (p) → get field 1 (i32 state), box as i31
    ((and (string=? op-str "%promise-state") (= len 2))
     (codegen-struct-get-i31! (ctx-ty-promise ctx) 1 val body ctx))

    ;; %promise-set! (p v) → set field 0
    ((and (string=? op-str "%promise-set!") (= len 3))
     (codegen-struct-set-void! (ctx-ty-promise ctx) 0 val body ctx))

    ;; %promise-set-state! (p v) → set field 1 (i32)
    ((and (string=? op-str "%promise-set-state!") (= len 3))
     (and (codegen-expr (cadr val) body ctx)
          (begin
            (emit-ref-cast! body (ctx-ty-promise ctx))
            (and (codegen-i32-arg (caddr val) body ctx)
                 (begin
                   (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-STRUCT-SET)
                   (wbuf-u32! body (ctx-ty-promise ctx)) (wbuf-u32! body 1)
                   (emit-void! body) #t)))))

    ;; promise? (x) → type predicate
    ((and (string=? op-str "promise?") (= len 2))
     (codegen-type-pred! (ctx-ty-promise ctx) val body ctx))

    ;; %call (fn-idx arg1 arg2 ...) → call function by raw index
    ((and (string=? op-str "%call") (>= len 2))
     (let ((fn-idx (cadr val))
           (ok #t))
       (let loop ((args (cddr val)))
         (when (and ok (pair? args))
           (set! ok (codegen-expr (car args) body ctx))
           (loop (cdr args))))
       (when ok
         (wbuf-byte! body (if tail? OP-RETURN-CALL OP-CALL))
         (wbuf-u32! body fn-idx))
       ok))

    ;; function call (known function) or dynamic call
    (else
     (codegen-function-call op-str val len body ctx tail?))))))

;;; --- Function call codegen ---
(define (codegen-function-call opname val len body ctx tail?)
  ;; Try known function call
  (let ((fn (ctx-func ctx opname)))
    (if (and fn (= len (+ (uf-nparams fn) 1)))
        (let ((ok #t))
          (let loop ((args (cdr val)))
            (when (and ok (pair? args))
              (set! ok (codegen-expr (car args) body ctx))
              (loop (cdr args))))
          (when ok
            (wbuf-byte! body (if tail? OP-RETURN-CALL OP-CALL))
            (wbuf-u32! body (uf-func-idx fn)))
          ok)
        ;; Try dynamic call (variable holding a closure)
        (let ((li (ctx-local ctx opname))
              (gi -1))
          (when (< li 0)
            (set! gi (ctx-global ctx opname)))
          (if (or (>= li 0) (>= gi 0))
              (let ((nargs (- len 1))
                    (ok #t)
                    (is-boxed (and (>= li 0) (ctx-is-boxed? ctx opname))))
                ;; Extract env from closure
                (if (>= li 0)
                    (begin (wbuf-byte! body OP-LOCAL-GET) (wbuf-u32! body li))
                    (begin (wbuf-byte! body OP-GLOBAL-GET) (wbuf-u32! body gi)))
                ;; Unbox if needed
                (when is-boxed
                  (emit-ref-cast! body TY-ENV)
                  (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 0)
                  (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-ARRAY-GET)
                  (wbuf-u32! body TY-ENV))
                (emit-cast-struct-get! body TY-CLOSURE 1)
                ;; Push args
                (let loop ((args (cdr val)))
                  (when (and ok (pair? args))
                    (set! ok (codegen-expr (car args) body ctx))
                    (loop (cdr args))))
                ;; Extract code from closure
                (when ok
                  (if (>= li 0)
                      (begin (wbuf-byte! body OP-LOCAL-GET) (wbuf-u32! body li))
                      (begin (wbuf-byte! body OP-GLOBAL-GET) (wbuf-u32! body gi)))
                  ;; Unbox if needed
                  (when is-boxed
                    (emit-ref-cast! body TY-ENV)
                    (wbuf-byte! body OP-I32-CONST) (wbuf-i32! body 0)
                    (wbuf-byte! body OP-GC-PREFIX) (wbuf-byte! body GC-ARRAY-GET)
                    (wbuf-u32! body TY-ENV))
                  (emit-cast-struct-get! body TY-CLOSURE 0)
                  ;; Find closure type index
                  (let ((tidx (let loop ((i 0) (cas (ctx-closure-arities ctx)))
                                (cond
                                  ((null? cas) -1)
                                  ((= (car cas) nargs)
                                   (+ (ctx-ty-user-start ctx) (ctx-narity ctx) i))
                                  (else (loop (+ i 1) (cdr cas)))))))
                    (if (< tidx 0)
                        (begin
                          (display (string-append "error: no closure type for arity "
                                                  (number->string nargs) "\n")
                                   (current-error-port))
                          (set! ok #f))
                        (begin
                          (wbuf-byte! body (if tail? OP-RETURN-CALL-INDIRECT OP-CALL-INDIRECT))
                          (wbuf-u32! body tidx)
                          (wbuf-u32! body 0)))))
                ok)
              ;; Unknown call
              (begin
                (display (string-append "error: cannot compile call to '"
                                        opname "' with "
                                        (number->string (- len 1)) " args\n")
                         (current-error-port))
                #f))))))

;;; --- Implicit begin (multi-expression body) ---

;;; codegen-body/tail — compile a sequence of expressions.
;;; Uses top-level recursion instead of named-let to avoid letrec
;;; re-entrancy issues when the compiler is compiled to WASM.
(define (codegen-body/tail exprs body ctx tail?)
  (if (not (pair? exprs))
      #t
      (codegen-body-loop exprs body ctx tail?)))

(define (codegen-body-loop es body ctx tail?)
  (let* ((rest (cdr es))
         (last? (null? rest))
         (ok (if last?
                 (codegen-expr/tail (car es) body ctx tail?)
                 (codegen-expr (car es) body ctx))))
    (if (not ok)
        #f
        (if last?
            #t
            (begin
              (wbuf-byte! body OP-DROP)
              (codegen-body-loop rest body ctx tail?))))))

(define (codegen-body exprs body ctx)
  (codegen-body/tail exprs body ctx #f))
