;;; codegen.scm — Module-level WASM codegen (replaces codegen.c)
;;; Uses wasm.scm buffers, calls codegen-expr for expression compilation,
;;; and emit-ffi for C runtime helper bodies.

;;; --- wrap-info: #(orig-fidx wrapper-fidx nparams) ---
(define (make-wrap-info orig wrapper np) (vector orig wrapper np))
(define (wi-orig-fidx wi) (vector-ref wi 0))
(define (wi-wrapper-fidx wi) (vector-ref wi 1))
(define (wi-nparams wi) (vector-ref wi 2))

;;; --- WIT export shim helpers ---

(define (wit-type-to-core-byte ty)
  ;; Convert a WIT type to a WASM core value type byte (single-value types only)
  (if (and (pair? ty) (eq? (car ty) 'prim))
      (let ((name (cadr ty)))
        (cond
          ((or (string=? name "bool") (string=? name "u8") (string=? name "s8")
               (string=? name "u16") (string=? name "s16")
               (string=? name "u32") (string=? name "s32")
               (string=? name "char"))
           TYPE-I32)
          ((or (string=? name "u64") (string=? name "s64"))
           TYPE-I64)
          ((string=? name "f64") TYPE-F64)
          (else TYPE-I32)))
      TYPE-I32))

(define (wit-core-type-byte sym)
  ;; Map a flat ABI type symbol to a WASM type byte
  (cond ((eq? sym 'i32) TYPE-I32)
        ((eq? sym 'i64) TYPE-I64)
        ((eq? sym 'f32) TYPE-F32)
        ((eq? sym 'f64) TYPE-F64)
        (else TYPE-I32)))

(define (find-user-func funcs name)
  ;; Find a user function by name, return uf or #f
  (let loop ((fs funcs))
    (cond
      ((null? fs) #f)
      ((and (uf-name (car fs)) (string=? (uf-name (car fs)) name))
       (car fs))
      (else (loop (cdr fs))))))

;;; --- Runtime function compilation ---

(define (compile-runtime-func! sec params extra-locals body-forms ctx)
  ;; Compile a runtime function body using codegen-body.
  ;; params: list of param name strings
  ;; extra-locals: list of extra local name strings (declared as eqref)
  ;; body-forms: list of Scheme expressions to compile
  ;; ctx: base codegen context (locals will be replaced)
  (let ((ubody (wbuf-make))
        (all-names (append params extra-locals))
        (nextra (length extra-locals)))
    ;; Write locals declaration
    (if (> nextra 0)
        (begin
          (wbuf-u32! ubody 1)
          (wbuf-u32! ubody nextra)
          (wbuf-byte! ubody HT-EQ))
        (wbuf-u32! ubody 0))
    ;; Create ctx with these locals
    (let* ((fn-ctx (ctx-with-locals ctx all-names (length all-names)))
           (ok (codegen-body/tail body-forms ubody fn-ctx #t)))
      (when ok
        (wbuf-byte! ubody OP-END)
        (wbuf-func-body! sec ubody))
      ok)))

(define (compile-rt! sec rt-def ctx)
  ;; Compile a runtime function from a definition list: (params locals body-forms)
  (compile-runtime-func! sec (car rt-def) (cadr rt-def) (caddr rt-def) ctx))

;;; --- Count let/let* locals needed for WASM local declarations ---

(define (count-let-locals forms)
  (let ((count 0))
    (define (scan expr)
      (when (pair? expr)
        (let ((head (car expr)))
          (cond
            ((not (symbol? head))
             (for-each scan expr))
            ((or (eq? head 'let) (eq? head 'let*))
             (when (and (>= (length expr) 3) (pair? (cadr expr)))
               (set! count (+ count (length (cadr expr))))
               (for-each (lambda (b)
                           (when (and (pair? b) (= (length b) 2))
                             (scan (cadr b))))
                         (cadr expr))
               (for-each scan (cddr expr))))
            ;; or with 3+ operands uses 1 temp local
            ((and (eq? head 'or) (> (length expr) 2))
             (set! count (+ count 1))
             (for-each scan (cdr expr)))
            ;; char-whitespace?/char-upcase/char-downcase use 1 temp local
            ((or (eq? head 'char-whitespace?)
                 (eq? head 'char-upcase)
                 (eq? head 'char-downcase))
             (set! count (+ count 1))
             (for-each scan (cdr expr)))
            ((eq? head 'lambda) #f)   ;; stop — separate function
            ((eq? head 'quote) #f)    ;; stop — data
            (else (for-each scan (cdr expr)))))))
    ;; Only scan non-define forms and var-define init exprs (not function bodies)
    (for-each (lambda (form)
                (cond
                  ((or (is-func-define? form) (is-external-define? form))
                   #f)  ;; skip — compiled as separate functions
                  ((is-var-define? form)
                   (scan (caddr form)))  ;; scan init expr only
                  (else
                   (scan form))))
              forms)
    count))

;;; --- Entry point ---

(define (codegen-module forms target analysis)
  (let* ((codegen-t0 (if *profile* (current-milliseconds) 0))
         (flags (ar-flags analysis))
         (m (ar-index-map analysis))
         ;; Read flags
         (needs-display (vector-ref flags FLAG-DISPLAY))
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
         (needs-callcc (vector-ref flags FLAG-CALLCC))
         ;; Read derived booleans from index-map
         (needs-dispatch (vector-ref m IDX-NEEDS-DISPATCH))
         (needs-eqv-type (vector-ref m IDX-NEEDS-EQV-TYPE))
         (needs-io (vector-ref m IDX-NEEDS-IO))
         (needs-memory (vector-ref m IDX-NEEDS-MEMORY))
         (needs-numstr (vector-ref m IDX-NEEDS-NUMSTR))
         ;; Read counts
         (nimports (vector-ref m IDX-NIMPORTS))
         (nbuiltins (vector-ref m IDX-NBUILTINS))
         ;; Read function indices from index-map
         (fn-proc-exit (vector-ref m IDX-FN-PROC-EXIT))
         (fn-start nimports)
         (fn-display (vector-ref m IDX-FN-DISPLAY))
         (fn-write (vector-ref m IDX-FN-WRITE))
         (fn-newline-fn (vector-ref m IDX-FN-NEWLINE))
         (fn-eqv (vector-ref m IDX-FN-EQV))
         (fn-equal (vector-ref m IDX-FN-EQUAL))
         (fn-num-to-str (vector-ref m IDX-FN-NUM-TO-STR))
         (fn-str-to-num (vector-ref m IDX-FN-STR-TO-NUM))
         (fn-flonum-start (vector-ref m IDX-FN-FLONUM-START))
         (fn-gcd (vector-ref m IDX-FN-GCD))
         (fn-make-complex (vector-ref m IDX-FN-MAKE-COMPLEX))
         (fn-math-start (vector-ref m IDX-FN-MATH-START))
         (fn-command-line (vector-ref m IDX-FN-COMMAND-LINE))
         (fn-string-append (vector-ref m IDX-FN-STRING-APPEND))
         (fn-linear-alloc (vector-ref m IDX-FN-LINEAR-ALLOC))
         (fn-bv-alloc (vector-ref m IDX-FN-BV-ALLOC))
         (fn-bv-copy-str (vector-ref m IDX-FN-BV-COPY-STR))
         (fn-ptr-to-str (vector-ref m IDX-FN-PTR-TO-STR))
         (fn-bv-alloc-fill (vector-ref m IDX-FN-BV-ALLOC-FILL))
         (fn-open-input-file (vector-ref m IDX-FN-OPEN-INPUT-FILE))
         (fn-open-output-file (vector-ref m IDX-FN-OPEN-OUTPUT-FILE))
         (fn-close-port (vector-ref m IDX-FN-CLOSE-PORT))
         (fn-read-char-fn (vector-ref m IDX-FN-READ-CHAR))
         (fn-peek-char-fn (vector-ref m IDX-FN-PEEK-CHAR))
         (fn-write-char-fn (vector-ref m IDX-FN-WRITE-CHAR))
         (fn-substring (vector-ref m IDX-FN-SUBSTRING))
         (fn-string-copy (vector-ref m IDX-FN-STRING-COPY))
         (fn-string-to-list (vector-ref m IDX-FN-STRING-TO-LIST))
         (fn-list-to-string (vector-ref m IDX-FN-LIST-TO-STRING))
         (fn-string-eq (vector-ref m IDX-FN-STRING-EQ))
         (fn-string-lt (vector-ref m IDX-FN-STRING-LT))
         (fn-string-ci-eq (vector-ref m IDX-FN-STRING-CI-EQ))
         (fn-string-ci-lt (vector-ref m IDX-FN-STRING-CI-LT))
         (fn-string-fill (vector-ref m IDX-FN-STRING-FILL))
         (fn-read (vector-ref m IDX-FN-READ))
         (fn-read-list (vector-ref m IDX-FN-READ-LIST))
         (fn-read-string (vector-ref m IDX-FN-READ-STRING))
         (fn-vector-copy (vector-ref m IDX-FN-VECTOR-COPY))
         (fn-list-to-vector (vector-ref m IDX-FN-LIST-TO-VECTOR))
         (fn-vector-to-list (vector-ref m IDX-FN-VECTOR-TO-LIST))
         (fn-vector-fill (vector-ref m IDX-FN-VECTOR-FILL))
         (fn-intern-sym (vector-ref m IDX-FN-INTERN-SYM))
         (fn-apply (vector-ref m IDX-FN-APPLY))
         (fn-file-exists (vector-ref m IDX-FN-FILE-EXISTS))
         (fn-bv-copy-range (vector-ref m IDX-FN-BV-COPY-RANGE))
         (fn-bv-copy (vector-ref m IDX-FN-BV-COPY))
         (fn-bv-copy-from (vector-ref m IDX-FN-BV-COPY-FROM))
         (fn-bv-append (vector-ref m IDX-FN-BV-APPEND))
         (fn-utf8-to-str-range (vector-ref m IDX-FN-UTF8-TO-STR-RANGE))
         (fn-utf8-to-str (vector-ref m IDX-FN-UTF8-TO-STR))
         (fn-utf8-to-str-from (vector-ref m IDX-FN-UTF8-TO-STR-FROM))
         (fn-str-to-utf8-range (vector-ref m IDX-FN-STR-TO-UTF8-RANGE))
         (fn-str-to-utf8 (vector-ref m IDX-FN-STR-TO-UTF8))
         (fn-str-to-utf8-from (vector-ref m IDX-FN-STR-TO-UTF8-FROM))
         (fn-get-env-var (vector-ref m IDX-FN-GET-ENV-VAR))
         (fn-get-env-vars (vector-ref m IDX-FN-GET-ENV-VARS))
         (fn-open-input-string (vector-ref m IDX-FN-OPEN-INPUT-STRING))
         (n-wit-imports (vector-ref m IDX-WIT-NIMPORTS))
         (fn-get-stdout (vector-ref m IDX-FN-GET-STDOUT))
         (fn-get-stderr (vector-ref m IDX-FN-GET-STDERR))
         (fn-stream-write (vector-ref m IDX-FN-STREAM-WRITE))
         (fn-clock-now (vector-ref m IDX-FN-CLOCK-NOW))
         (fn-get-arguments (vector-ref m IDX-FN-GET-ARGUMENTS))
         (fn-get-environment (vector-ref m IDX-FN-GET-ENVIRONMENT))
         (fn-get-stdin (vector-ref m IDX-FN-GET-STDIN))
         (fn-stream-read (vector-ref m IDX-FN-STREAM-READ))
         (fn-get-directories (vector-ref m IDX-FN-GET-DIRECTORIES))
         (fn-open-at (vector-ref m IDX-FN-OPEN-AT))
         (fn-read-via-stream (vector-ref m IDX-FN-READ-VIA-STREAM))
         (fn-write-via-stream (vector-ref m IDX-FN-WRITE-VIA-STREAM))
         (fn-drop-descriptor (vector-ref m IDX-FN-DROP-DESCRIPTOR))
         (fn-drop-input-stream (vector-ref m IDX-FN-DROP-INPUT-STREAM))
         (fn-drop-output-stream (vector-ref m IDX-FN-DROP-OUTPUT-STREAM))
         (ty-stream-read (vector-ref m IDX-TY-STREAM-READ))
         (ty-open-at (vector-ref m IDX-TY-OPEN-AT))
         (ty-void-i32 (vector-ref m IDX-TY-VOID-I32))
         (ty-void-i64 (vector-ref m IDX-TY-VOID-I64))
         (fn-user-start (vector-ref m IDX-FN-USER-START))
         ;; Read type indices from index-map
         (ty-char (vector-ref m IDX-TY-CHAR))
         (ty-eqv (vector-ref m IDX-TY-EQV))
         (ty-symbol (vector-ref m IDX-TY-SYMBOL))
         (ty-numstr (vector-ref m IDX-TY-NUMSTR))
         (ty-flonum (vector-ref m IDX-TY-FLONUM))
         (ty-rational (vector-ref m IDX-TY-RATIONAL))
         (ty-complex (vector-ref m IDX-TY-COMPLEX))
         (ty-args-import (vector-ref m IDX-TY-ARGS-IMPORT))
         (ty-void-eqref (vector-ref m IDX-TY-VOID-EQREF))
         (ty-bytevector (vector-ref m IDX-TY-BYTEVECTOR))
         (ty-bv-copy-str (vector-ref m IDX-TY-BV-COPY-STR))
         (ty-port (vector-ref m IDX-TY-PORT))
         (ty-eof (vector-ref m IDX-TY-EOF))
         (ty-str-3arg (vector-ref m IDX-TY-STR-3ARG))
         (ty-clock-import (vector-ref m IDX-TY-CLOCK-IMPORT))
         (ty-vector (vector-ref m IDX-TY-VECTOR))
         (ty-promise (vector-ref m IDX-TY-PROMISE))
         (ty-user-start (vector-ref m IDX-TY-USER-START)))

      ;; Get pre-computed data from analysis
      (let* ((funcs (ar-funcs analysis))
             (globals (ar-globals analysis))
             (arities-vec (ar-arities analysis))
             (narity (vector-ref arities-vec 64))
             (arities (let loop ((i 0) (acc '()))
                        (if (>= i narity) (reverse acc)
                            (loop (+ i 1) (cons (vector-ref arities-vec i) acc)))))
             (externals (ar-externals analysis))
             (lambdas (ar-lambdas analysis))
             (boxed-vars (ar-boxed-vars analysis))
             (nglobals (length globals))
             (nexternals (length externals))
             (nlambdas (length lambdas))
             (closure-arities '())
             (nclosure-arity 0))

        ;; Compute closure arities from is_closure=true lambdas
        (for-each
         (lambda (ll)
           (when (ll-is-closure ll)
             (let ((n (ll-noriginal ll)))
               (unless (member n closure-arities)
                 (set! closure-arities (append closure-arities (list n)))
                 (set! nclosure-arity (+ nclosure-arity 1))))))
         lambdas)

        ;; Ensure closure arities for named functions (for wrappers)
        (let ((orig-nfuncs (length funcs)))
          (let loop ((i 0) (fs funcs))
            (when (and (pair? fs) (< i orig-nfuncs))
              (let ((uf (car fs)))
                (when (uf-name uf)
                  (let ((np (uf-nparams uf)))
                    (unless (member np closure-arities)
                      (set! closure-arities (append closure-arities (list np)))
                      (set! nclosure-arity (+ nclosure-arity 1))))))
              (loop (+ i 1) (cdr fs))))

          ;; Fix up closure lambdas
          (for-each
           (lambda (ll)
             (when (ll-is-closure ll)
               (let* ((n (ll-noriginal ll))
                      (ctidx (let loop ((j 0) (cas closure-arities))
                               (cond ((null? cas) -1)
                                     ((= (car cas) n) (+ ty-user-start narity j))
                                     (else (loop (+ j 1) (cdr cas)))))))
                 (ll-set-type-idx! ll ctidx)
                 ;; Find matching user-func and update
                 (for-each
                  (lambda (uf)
                    (when (= (uf-func-idx uf) (ll-func-idx ll))
                      (uf-set-type-idx! uf ctidx)
                      ;; Rebuild param_names: [__env__ orig_params... free_vars...]
                      (let* ((lam (ll-form ll))
                             (params (cadr lam))
                             (nfree (ll-nfree ll))
                             (np (cons "__env__"
                                       (append (map (lambda (p) (symbol->string p)) params)
                                               (ll-free-vars ll)))))
                        (uf-set-param-names! uf np)
                        (uf-set-nparams! uf (+ 1 n)))))
                  funcs))))
           lambdas)

          ;; Generate closure wrappers for named functions
          (let ((wrappers '())
                (nwrappers 0))

            (let loop ((i 0) (fs funcs))
              (when (and (pair? fs) (< i orig-nfuncs))
                (let ((uf (car fs)))
                  (when (uf-name uf)
                    (let* ((np (uf-nparams uf))
                           (orig-fidx (uf-func-idx uf))
                           (ctidx (let loop ((j 0) (cas closure-arities))
                                    (cond ((null? cas) -1)
                                          ((= (car cas) np) (+ ty-user-start narity j))
                                          (else (loop (+ j 1) (cdr cas))))))
                           (wi (length funcs))
                           (wrapper-fidx (+ fn-user-start wi))
                           (new-uf (make-user-func #f wrapper-fidx ctidx (+ 1 np)
                                                   '() '() 0 #f 0)))
                      (set! funcs (append funcs (list new-uf)))
                      (set! wrappers (append wrappers (list (make-wrap-info orig-fidx wrapper-fidx np))))
                      (set! nwrappers (+ nwrappers 1)))))
                (loop (+ i 1) (cdr fs))))

            ;; Ensure arity 1 is in closure-arities for escape_cont (when needs-callcc)
            (when needs-callcc
              (unless (member 1 closure-arities)
                (set! closure-arities (append closure-arities (list 1)))
                (set! nclosure-arity (+ nclosure-arity 1))))

            ;; Create external FFI wrapper functions
            (let ((nexternal-types 0)
                  (ext-type-base (+ ty-user-start narity nclosure-arity))
                  (escape-cont-table-idx -1))

              (for-each
               (lambda (ext)
                 (let ((i (let loop ((j 0) (es externals))
                            (cond ((null? es) 0)
                                  ((eq? (car es) ext) j)
                                  (else (loop (+ j 1) (cdr es)))))))
                   ;; Deduplicate wrapper type by signature
                   (let ((found (let loop ((j 0) (es externals))
                                  (cond
                                    ((>= j i) -1)
                                    ((and (= (ext-nparams (car es)) (ext-nparams ext))
                                          (= (ext-return-type (car es)) (ext-return-type ext))
                                          (equal? (ext-param-types (car es)) (ext-param-types ext)))
                                     (ext-wrapper-type-idx (car es)))
                                    (else (loop (+ j 1) (cdr es)))))))
                     (if (>= found 0)
                         (ext-set-wrapper-type-idx! ext found)
                         (begin
                           (ext-set-wrapper-type-idx! ext (+ ext-type-base nexternal-types))
                           (set! nexternal-types (+ nexternal-types 1)))))
                   ;; Create wrapper function
                   (let* ((wi (length funcs))
                          (wrapper-fidx (+ fn-user-start wi))
                          (new-uf (make-user-func #f wrapper-fidx
                                                  (ext-wrapper-type-idx ext)
                                                  (ext-nparams ext) '() '() 0 #f 0)))
                     (ext-set-wrapper-func-idx! ext wrapper-fidx)
                     (set! funcs (append funcs (list new-uf))))))
               externals)

              ;; Add escape_cont synthetic function when needs-callcc
              (when needs-callcc
                (let* ((callcc-ctidx (let loop ((j 0) (cas closure-arities))
                                       (cond ((null? cas) -1)
                                             ((= (car cas) 1) (+ ty-user-start narity j))
                                             (else (loop (+ j 1) (cdr cas))))))
                       (wi (length funcs))
                       (escape-cont-fidx (+ fn-user-start wi))
                       (new-uf (make-user-func #f escape-cont-fidx callcc-ctidx 2
                                               '("__env__" "__k__") #f 0 #f 0)))
                  (set! escape-cont-table-idx wi)
                  (set! funcs (append funcs (list new-uf)))))

              (let* ((nfuncs (length funcs))
                     ;; WIT export shims: collect info for each WIT export
                     (wit-exports (if *wit-world*
                                      (wit-world-func-exports *wit-world*)
                                      '()))
                     (wit-imports (if *wit-world*
                                     (wit-world-func-imports *wit-world*)
                                     '()))
                     (n-wit-shims (length wit-exports))
                     (wit-needs-memory
                      (let loop ((fs (append wit-imports wit-exports)))
                        (and (pair? fs)
                             (or (wit-func-needs-memory? (car fs))
                                 (loop (cdr fs))))))
                     ;; WIT shim type indices start after external types
                     (wit-shim-type-base (+ ext-type-base nexternal-types))
                     ;; WIT shim function indices start after all user/wrapper funcs
                     (wit-shim-func-base (+ fn-user-start nfuncs))
                     ;; WIT import type indices start after export shim types
                     (wit-import-type-base (+ wit-shim-type-base n-wit-shims))
                     ;; Build WIT shim info: list of (name user-uf wit-func type-idx func-idx)
                     (wit-shim-info
                      (let loop ((wfs wit-exports) (i 0) (acc '()))
                        (if (null? wfs)
                            (reverse acc)
                            (let* ((wf (car wfs))
                                   (name (wit-func-name wf))
                                   (uf (find-user-func funcs name)))
                              (if (not uf)
                                  (begin
                                    (display (string-append "error: WIT export '"
                                                            name "' not found in source\n")
                                             (current-error-port))
                                    (exit 1))
                                  (loop (cdr wfs) (+ i 1)
                                        (cons (vector name uf wf
                                                      (+ wit-shim-type-base i)
                                                      (+ wit-shim-func-base i))
                                              acc)))))))
                     ;; Build WIT import info: list of #(name wrapper-uf wit-func import-type-idx import-func-idx)
                     ;; import-func-idx = the core module import index (0..n-1)
                     ;; wrapper-uf = the synthetic user-func that wraps this import
                     (wit-import-info
                      (let loop ((wfs wit-imports) (i 0) (acc '()))
                        (if (null? wfs)
                            (reverse acc)
                            (let* ((wf (car wfs))
                                   (name (wit-func-name wf))
                                   (uf (find-user-func funcs name)))
                              (loop (cdr wfs) (+ i 1)
                                    (cons (vector name uf wf
                                                  (+ wit-import-type-base i)
                                                  i) ;; import func index
                                          acc)))))))

                ;; === Emit module ===
                (let ((out (wbuf-make))
                      (sec (wbuf-make)))

                  (wbuf-bytes-list! out WASM-HEADER)

                  ;; === Type section ===
                  (wbuf-reset! sec)
                  (wbuf-u32! sec (+ ty-user-start narity nclosure-arity nexternal-types n-wit-shims n-wit-imports))

                  ;; type 0 (TY_ENV): (array (mut (ref null eq)))
                  (wbuf-byte! sec COMP-ARRAY)
                  (wbuf-byte! sec REF-NULL-TYPE) (wbuf-byte! sec HT-EQ)
                  (wbuf-byte! sec FIELD-MUT)
                  ;; type 1 (TY_CLOSURE): (struct i32 (ref $env))
                  (wbuf-byte! sec COMP-STRUCT)
                  (wbuf-u32! sec 2)
                  (wbuf-byte! sec TYPE-I32) (wbuf-byte! sec FIELD-CONST)
                  (wbuf-byte! sec REF-TYPE) (wbuf-u32! sec TY-ENV) (wbuf-byte! sec FIELD-CONST)
                  ;; type 2 (TY_I32_VOID)
                  (wbuf-byte! sec TYPE-FUNC) (wbuf-u32! sec 1) (wbuf-byte! sec TYPE-I32) (wbuf-u32! sec 0)
                  ;; type 3 (TY_VOID_VOID)
                  (wbuf-byte! sec TYPE-FUNC) (wbuf-u32! sec 0) (wbuf-u32! sec 0)
                  ;; type 4 (TY_IO_IMPORT)
                  ;; P2: (i32,i32,i32,i32)->void for stream-write
                  ;; TY_IO_IMPORT: (i32,i32,i32,i32)->void for stream-write
                  (wbuf-byte! sec TYPE-FUNC) (wbuf-u32! sec 4)
                  (wbuf-byte! sec TYPE-I32) (wbuf-byte! sec TYPE-I32)
                  (wbuf-byte! sec TYPE-I32) (wbuf-byte! sec TYPE-I32)
                  (wbuf-u32! sec 0)
                  ;; type 5 (TY_PAIR)
                  (wbuf-byte! sec COMP-STRUCT) (wbuf-u32! sec 2)
                  (wbuf-byte! sec REF-NULL-TYPE) (wbuf-byte! sec HT-EQ) (wbuf-byte! sec FIELD-MUT)
                  (wbuf-byte! sec REF-NULL-TYPE) (wbuf-byte! sec HT-EQ) (wbuf-byte! sec FIELD-MUT)
                  ;; type 6 (TY_STRING): (array (mut i8))
                  (wbuf-byte! sec COMP-ARRAY) (wbuf-byte! sec PACKED-I8) (wbuf-byte! sec FIELD-MUT)
                  ;; type 7 (TY_EQ_VOID)
                  (wbuf-byte! sec TYPE-FUNC) (wbuf-u32! sec 1) (wbuf-byte! sec HT-EQ) (wbuf-u32! sec 0)

                  ;; Conditional types
                  (when needs-char
                    (wbuf-byte! sec COMP-STRUCT) (wbuf-u32! sec 1)
                    (wbuf-byte! sec TYPE-I32) (wbuf-byte! sec FIELD-CONST))
                  (when needs-eqv-type
                    (wbuf-byte! sec TYPE-FUNC) (wbuf-u32! sec 2)
                    (wbuf-byte! sec HT-EQ) (wbuf-byte! sec HT-EQ)
                    (wbuf-u32! sec 1) (wbuf-byte! sec HT-EQ))
                  (when needs-symbol
                    (wbuf-byte! sec COMP-STRUCT) (wbuf-u32! sec 1)
                    (wbuf-byte! sec REF-TYPE) (wbuf-u32! sec TY-STRING) (wbuf-byte! sec FIELD-CONST))
                  (when needs-numstr
                    (wbuf-byte! sec TYPE-FUNC) (wbuf-u32! sec 1) (wbuf-byte! sec HT-EQ)
                    (wbuf-u32! sec 1) (wbuf-byte! sec HT-EQ))
                  (when needs-flonum
                    (wbuf-byte! sec COMP-STRUCT) (wbuf-u32! sec 1)
                    (wbuf-byte! sec TYPE-F64) (wbuf-byte! sec FIELD-CONST))
                  (when needs-rational
                    (wbuf-byte! sec COMP-STRUCT) (wbuf-u32! sec 2)
                    (wbuf-byte! sec TYPE-I32) (wbuf-byte! sec FIELD-CONST)
                    (wbuf-byte! sec TYPE-I32) (wbuf-byte! sec FIELD-CONST))
                  (when needs-complex
                    (wbuf-byte! sec COMP-STRUCT) (wbuf-u32! sec 2)
                    (wbuf-byte! sec REF-NULL-TYPE) (wbuf-byte! sec HT-EQ) (wbuf-byte! sec FIELD-CONST)
                    (wbuf-byte! sec REF-NULL-TYPE) (wbuf-byte! sec HT-EQ) (wbuf-byte! sec FIELD-CONST))
                  (when (or needs-command-line needs-get-env)
                    (wbuf-byte! sec TYPE-FUNC) (wbuf-u32! sec 2)
                    (wbuf-byte! sec TYPE-I32) (wbuf-byte! sec TYPE-I32)
                    (wbuf-u32! sec 1) (wbuf-byte! sec TYPE-I32))
                  (when (or needs-newline needs-command-line needs-get-env)
                    (wbuf-byte! sec TYPE-FUNC) (wbuf-u32! sec 0)
                    (wbuf-u32! sec 1) (wbuf-byte! sec HT-EQ))
                  (when needs-bytevector
                    (wbuf-byte! sec COMP-STRUCT) (wbuf-u32! sec 2)
                    (wbuf-byte! sec TYPE-I32) (wbuf-byte! sec FIELD-CONST)
                    (wbuf-byte! sec TYPE-I32) (wbuf-byte! sec FIELD-CONST)
                    (wbuf-byte! sec TYPE-FUNC) (wbuf-u32! sec 3)
                    (wbuf-byte! sec HT-EQ) (wbuf-byte! sec HT-EQ) (wbuf-byte! sec HT-EQ)
                    (wbuf-u32! sec 1) (wbuf-byte! sec HT-EQ))

                  (when needs-file-io
                    ;; ty-port: (struct i32 i32 (mut i32) (ref null eq) (mut i32))
                    (wbuf-byte! sec COMP-STRUCT) (wbuf-u32! sec 5)
                    (wbuf-byte! sec TYPE-I32) (wbuf-byte! sec FIELD-CONST)
                    (wbuf-byte! sec TYPE-I32) (wbuf-byte! sec FIELD-CONST)
                    (wbuf-byte! sec TYPE-I32) (wbuf-byte! sec FIELD-MUT)
                    (wbuf-byte! sec REF-NULL-TYPE) (wbuf-byte! sec HT-EQ) (wbuf-byte! sec FIELD-CONST)
                    (wbuf-byte! sec TYPE-I32) (wbuf-byte! sec FIELD-MUT)
                    ;; ty-eof: (struct) — zero fields
                    (wbuf-byte! sec COMP-STRUCT) (wbuf-u32! sec 0)
                    ;; ty-stream-read: (i32, i64, i32) -> ()
                    (wbuf-byte! sec TYPE-FUNC) (wbuf-u32! sec 3)
                    (wbuf-byte! sec TYPE-I32) (wbuf-byte! sec TYPE-I64) (wbuf-byte! sec TYPE-I32)
                    (wbuf-u32! sec 0)
                    ;; ty-open-at: (i32*7) -> ()
                    (wbuf-byte! sec TYPE-FUNC) (wbuf-u32! sec 7)
                    (wbuf-byte! sec TYPE-I32) (wbuf-byte! sec TYPE-I32)
                    (wbuf-byte! sec TYPE-I32) (wbuf-byte! sec TYPE-I32)
                    (wbuf-byte! sec TYPE-I32) (wbuf-byte! sec TYPE-I32)
                    (wbuf-byte! sec TYPE-I32)
                    (wbuf-u32! sec 0))

                  (when needs-string-ops
                    ;; ty-str-3arg: (func (eqref eqref eqref) -> (eqref))
                    (wbuf-byte! sec TYPE-FUNC) (wbuf-u32! sec 3)
                    (wbuf-byte! sec HT-EQ) (wbuf-byte! sec HT-EQ) (wbuf-byte! sec HT-EQ)
                    (wbuf-u32! sec 1) (wbuf-byte! sec HT-EQ))

                  (when needs-clock
                    ;; ty-clock-import: (func (i32 i64 i32) -> (i32))
                    (wbuf-byte! sec TYPE-FUNC) (wbuf-u32! sec 3)
                    (wbuf-byte! sec TYPE-I32) (wbuf-byte! sec TYPE-I64) (wbuf-byte! sec TYPE-I32)
                    (wbuf-u32! sec 1) (wbuf-byte! sec TYPE-I32))

                  ;; ()->i32 type for run function and get-stdout/get-stderr
                  (when (not *wit-world*)
                    (wbuf-byte! sec TYPE-FUNC) (wbuf-u32! sec 0)
                    (wbuf-u32! sec 1) (wbuf-byte! sec TYPE-I32))

                  ;; ()->i64 type for clock-now
                  (when needs-clock
                    (wbuf-byte! sec TYPE-FUNC) (wbuf-u32! sec 0)
                    (wbuf-u32! sec 1) (wbuf-byte! sec TYPE-I64))

                  ;; ty-promise: (struct (mut (ref null eq)) (mut i32))
                  (when needs-promise
                    (wbuf-byte! sec COMP-STRUCT) (wbuf-u32! sec 2)
                    (wbuf-byte! sec REF-NULL-TYPE) (wbuf-byte! sec HT-EQ) (wbuf-byte! sec FIELD-MUT)
                    (wbuf-byte! sec TYPE-I32) (wbuf-byte! sec FIELD-MUT))

                  ;; ty-vector now aliases TY-ENV (same shape), no separate type needed

                  ;; User function types
                  (for-each
                   (lambda (arity)
                     (wbuf-byte! sec TYPE-FUNC)
                     (wbuf-u32! sec arity)
                     (let loop ((j 0)) (when (< j arity) (wbuf-byte! sec HT-EQ) (loop (+ j 1))))
                     (wbuf-u32! sec 1) (wbuf-byte! sec HT-EQ))
                   arities)

                  ;; Closure function types
                  (for-each
                   (lambda (ca)
                     (wbuf-byte! sec TYPE-FUNC)
                     (wbuf-u32! sec (+ 1 ca))
                     (wbuf-byte! sec REF-TYPE) (wbuf-u32! sec TY-ENV)
                     (let loop ((j 0)) (when (< j ca) (wbuf-byte! sec HT-EQ) (loop (+ j 1))))
                     (wbuf-u32! sec 1) (wbuf-byte! sec HT-EQ))
                   closure-arities)

                  ;; External wrapper types
                  (let loop ((i 0) (es externals))
                    (when (pair? es)
                      (let ((ext (car es)))
                        (let ((is-first (let loop2 ((j 0) (es2 externals))
                                          (cond ((>= j i) #t)
                                                ((= (ext-wrapper-type-idx (car es2))
                                                    (ext-wrapper-type-idx ext)) #f)
                                                (else (loop2 (+ j 1) (cdr es2)))))))
                          (when is-first
                            (wbuf-byte! sec TYPE-FUNC)
                            (wbuf-u32! sec (ext-nparams ext))
                            (for-each (lambda (pt) (wbuf-byte! sec pt)) (ext-param-types ext))
                            (if (= (ext-return-type ext) 0)
                                (wbuf-u32! sec 0)
                                (begin
                                  (wbuf-u32! sec 1)
                                  (wbuf-byte! sec (ext-return-type ext)))))))
                      (loop (+ i 1) (cdr es))))

                  ;; WIT export shim types (flat ABI)
                  (for-each
                   (lambda (ws)
                     (let* ((wf (vector-ref ws 2))
                            (params (wit-func-params wf))
                            (result (wit-func-result wf))
                            (flat-param-count
                             (let loop ((ps params) (n 0))
                               (if (null? ps) n
                                   (loop (cdr ps) (+ n (length (wit-type-core-types (cdar ps))))))))
                            (flat-result-types (if result (wit-type-core-types result) '())))
                       (wbuf-byte! sec TYPE-FUNC)
                       (wbuf-u32! sec flat-param-count)
                       (for-each
                        (lambda (p)
                          (for-each (lambda (ft) (wbuf-byte! sec (wit-core-type-byte ft)))
                                    (wit-type-core-types (cdr p))))
                        params)
                       (wbuf-u32! sec (length flat-result-types))
                       (for-each (lambda (ft) (wbuf-byte! sec (wit-core-type-byte ft)))
                                 flat-result-types)))
                   wit-shim-info)

                  ;; WIT import flat-ABI types
                  (for-each
                   (lambda (wi)
                     (let* ((wf (vector-ref wi 2))
                            (params (wit-func-params wf))
                            (result (wit-func-result wf))
                            (flat-param-count
                             (let loop ((ps params) (n 0))
                               (if (null? ps) n
                                   (loop (cdr ps) (+ n (length (wit-type-core-types (cdar ps))))))))
                            (flat-result-types (if result (wit-type-core-types result) '())))
                       (wbuf-byte! sec TYPE-FUNC)
                       (wbuf-u32! sec flat-param-count)
                       (for-each
                        (lambda (p)
                          (for-each (lambda (ft) (wbuf-byte! sec (wit-core-type-byte ft)))
                                    (wit-type-core-types (cdr p))))
                        params)
                       (wbuf-u32! sec (length flat-result-types))
                       (for-each (lambda (ft) (wbuf-byte! sec (wit-core-type-byte ft)))
                                 flat-result-types)))
                   wit-import-info)

                  (wbuf-section! out SEC-TYPE sec)

                  ;; === Import section ===
                  (let ((total-imports (+ nimports (if *wit-world* 0 1))))
                    (when (> total-imports 0)
                      (wbuf-reset! sec)
                      (wbuf-u32! sec total-imports)
                      (if *wit-world*
                        (for-each
                          (lambda (wi)
                            (wbuf-name! sec "env")
                            (wbuf-name! sec (vector-ref wi 0))
                            (wbuf-byte! sec #x00) (wbuf-u32! sec (vector-ref wi 3)))
                          wit-import-info)
                        (begin
                         ;; Memory from "mem", functions from "env"
                         (wbuf-name! sec "mem") (wbuf-name! sec "memory")
                         (wbuf-byte! sec #x02) (wbuf-byte! sec #x00) (wbuf-u32! sec 1)
                         (when needs-exit
                           (wbuf-name! sec "env") (wbuf-name! sec "exit")
                           (wbuf-byte! sec #x00) (wbuf-u32! sec TY-I32-VOID))
                         (when needs-io
                           (wbuf-name! sec "env") (wbuf-name! sec "get-stdout")
                           (wbuf-byte! sec #x00) (wbuf-u32! sec ty-void-i32)
                           (wbuf-name! sec "env") (wbuf-name! sec "get-stderr")
                           (wbuf-byte! sec #x00) (wbuf-u32! sec ty-void-i32)
                           (wbuf-name! sec "env") (wbuf-name! sec "stream-write")
                           (wbuf-byte! sec #x00) (wbuf-u32! sec TY-IO-IMPORT))
                         (when needs-clock
                           (wbuf-name! sec "env") (wbuf-name! sec "clock-now")
                           (wbuf-byte! sec #x00) (wbuf-u32! sec ty-void-i64))
                         (when needs-command-line
                           (wbuf-name! sec "env") (wbuf-name! sec "get-arguments")
                           (wbuf-byte! sec #x00) (wbuf-u32! sec TY-I32-VOID))
                         (when needs-get-env
                           (wbuf-name! sec "env") (wbuf-name! sec "get-environment")
                           (wbuf-byte! sec #x00) (wbuf-u32! sec TY-I32-VOID))
                         (when needs-file-io
                           (wbuf-name! sec "env") (wbuf-name! sec "get-stdin")
                           (wbuf-byte! sec #x00) (wbuf-u32! sec ty-void-i32)
                           (wbuf-name! sec "env") (wbuf-name! sec "stream-read")
                           (wbuf-byte! sec #x00) (wbuf-u32! sec ty-stream-read)
                           (wbuf-name! sec "env") (wbuf-name! sec "get-directories")
                           (wbuf-byte! sec #x00) (wbuf-u32! sec TY-I32-VOID)
                           (wbuf-name! sec "env") (wbuf-name! sec "open-at")
                           (wbuf-byte! sec #x00) (wbuf-u32! sec ty-open-at)
                           (wbuf-name! sec "env") (wbuf-name! sec "read-via-stream")
                           (wbuf-byte! sec #x00) (wbuf-u32! sec ty-stream-read)
                           (wbuf-name! sec "env") (wbuf-name! sec "write-via-stream")
                           (wbuf-byte! sec #x00) (wbuf-u32! sec ty-stream-read)
                           (wbuf-name! sec "env") (wbuf-name! sec "drop-descriptor")
                           (wbuf-byte! sec #x00) (wbuf-u32! sec TY-I32-VOID)
                           (wbuf-name! sec "env") (wbuf-name! sec "drop-input-stream")
                           (wbuf-byte! sec #x00) (wbuf-u32! sec TY-I32-VOID)
                           (wbuf-name! sec "env") (wbuf-name! sec "drop-output-stream")
                           (wbuf-byte! sec #x00) (wbuf-u32! sec TY-I32-VOID))))
                      (wbuf-section! out SEC-IMPORT sec)))

                  ;; === Function section ===
                  (wbuf-reset! sec)
                  (wbuf-u32! sec (+ nbuiltins nfuncs n-wit-shims (if *wit-world* 0 1)))
                  (wbuf-u32! sec TY-VOID-VOID)
                  (when needs-display (wbuf-u32! sec ty-eqv))
                  (when needs-write (wbuf-u32! sec ty-eqv))
                  (when needs-newline (wbuf-u32! sec ty-void-eqref))
                  (when needs-eqv (wbuf-u32! sec ty-eqv))
                  (when needs-equal (wbuf-u32! sec ty-eqv))
                  (when needs-num-to-str (wbuf-u32! sec ty-numstr))
                  (when needs-str-to-num (wbuf-u32! sec ty-numstr))
                  (when needs-dispatch
                    (let loop ((i 0)) (when (< i 9) (wbuf-u32! sec ty-eqv) (loop (+ i 1))))
                    (let loop ((i 0)) (when (< i 6) (wbuf-u32! sec ty-numstr) (loop (+ i 1)))))
                  (when needs-rational (wbuf-u32! sec ty-eqv))
                  (when needs-complex (wbuf-u32! sec ty-eqv))
                  (when needs-math
                    (let loop ((i 0)) (when (< i MATH-UNARY-COUNT) (wbuf-u32! sec ty-numstr) (loop (+ i 1))))
                    (wbuf-u32! sec ty-eqv)  ;; atan2
                    (wbuf-u32! sec ty-eqv)) ;; expt
                  (when needs-command-line (wbuf-u32! sec ty-void-eqref))
                  (when needs-string-append (wbuf-u32! sec ty-eqv))
                  (when needs-bytevector
                    (wbuf-u32! sec ty-eqv)            ;; linear-alloc
                    (wbuf-u32! sec ty-numstr)         ;; bv-alloc
                    (wbuf-u32! sec ty-bv-copy-str)    ;; bv-copy-str
                    (wbuf-u32! sec ty-eqv)            ;; ptr-to-str
                    (wbuf-u32! sec ty-eqv))           ;; bv-alloc-fill
                  (when needs-bv-copy
                    (wbuf-u32! sec ty-bv-copy-str)    ;; bv-copy-range
                    (wbuf-u32! sec ty-numstr)         ;; bv-copy
                    (wbuf-u32! sec ty-eqv))           ;; bv-copy-from
                  (when needs-bv-append
                    (wbuf-u32! sec ty-eqv))           ;; bv-append
                  (when needs-utf8-string
                    (wbuf-u32! sec ty-bv-copy-str)    ;; utf8-to-str-range
                    (wbuf-u32! sec ty-numstr)         ;; utf8-to-str
                    (wbuf-u32! sec ty-eqv)            ;; utf8-to-str-from
                    (wbuf-u32! sec ty-bv-copy-str)    ;; str-to-utf8-range
                    (wbuf-u32! sec ty-numstr)         ;; str-to-utf8
                    (wbuf-u32! sec ty-eqv))           ;; str-to-utf8-from
                  (when needs-file-io
                    (wbuf-u32! sec ty-numstr)   ;; open-input-file
                    (wbuf-u32! sec ty-numstr)   ;; open-output-file
                    (wbuf-u32! sec ty-numstr)   ;; close-port
                    (wbuf-u32! sec ty-numstr)   ;; read-char
                    (wbuf-u32! sec ty-numstr)   ;; peek-char
                    (wbuf-u32! sec ty-eqv)      ;; write-char (2 args)
                    (wbuf-u32! sec ty-numstr))  ;; open-input-string (1 arg)
                  (when needs-string-ops
                    (wbuf-u32! sec ty-str-3arg)  ;; substring (3 args)
                    (wbuf-u32! sec ty-numstr)    ;; string-copy (1 arg)
                    (wbuf-u32! sec ty-numstr)    ;; string->list (1 arg)
                    (wbuf-u32! sec ty-numstr)    ;; list->string (1 arg)
                    (wbuf-u32! sec ty-eqv)       ;; string=? (2 args)
                    (wbuf-u32! sec ty-eqv)       ;; string<? (2 args)
                    (wbuf-u32! sec ty-eqv)       ;; string-ci=? (2 args)
                    (wbuf-u32! sec ty-eqv)       ;; string-ci<? (2 args)
                    (wbuf-u32! sec ty-eqv))      ;; string-fill! (2 args)
                  (when needs-read
                    (wbuf-u32! sec ty-numstr)    ;; read (1 arg)
                    (wbuf-u32! sec ty-numstr)    ;; read-list (1 arg)
                    (wbuf-u32! sec ty-numstr))   ;; read-string (1 arg)
                  (when needs-vector
                    (wbuf-u32! sec ty-numstr)    ;; vector-copy (1 arg)
                    (wbuf-u32! sec ty-numstr)    ;; list->vector (1 arg)
                    (wbuf-u32! sec ty-numstr)    ;; vector->list (1 arg)
                    (wbuf-u32! sec ty-eqv))      ;; vector-fill! (2 args)
                  (when needs-symbol
                    (wbuf-u32! sec ty-numstr))   ;; intern-sym (1 arg)
                  (when needs-apply
                    (wbuf-u32! sec ty-eqv))      ;; apply (2 args)
                  (when needs-file-exists
                    (wbuf-u32! sec ty-numstr))    ;; file-exists? (1 arg)
                  (when needs-get-env
                    (wbuf-u32! sec ty-numstr)       ;; get-env-var (1 arg -> eqref)
                    (wbuf-u32! sec ty-void-eqref))  ;; get-env-vars (0 args -> eqref)
                  (for-each (lambda (uf) (wbuf-u32! sec (uf-type-idx uf))) funcs)
                  ;; WIT export shim function entries
                  (for-each
                   (lambda (ws) (wbuf-u32! sec (vector-ref ws 3)))  ;; type-idx
                   wit-shim-info)
                  ;; "run" function entry (()->i32) — only for WASI, not WIT
                  (when (not *wit-world*)
                    (wbuf-u32! sec ty-void-i32))
                  (wbuf-section! out SEC-FUNCTION sec)

                  ;; === Table section ===
                  (when (> nfuncs 0)
                    (wbuf-reset! sec)
                    (wbuf-u32! sec 1)
                    (wbuf-byte! sec TYPE-FUNCREF)
                    (wbuf-byte! sec #x00)
                    (wbuf-u32! sec nfuncs)
                    (wbuf-section! out SEC-TABLE sec))

                  ;; Memory section: WIT modules define their own, WASI imports from component
                  (when *wit-world*
                    (wbuf-reset! sec)
                    (wbuf-u32! sec 1)
                    (wbuf-byte! sec #x00)
                    (wbuf-u32! sec 1)
                    (wbuf-section! out SEC-MEMORY sec))

                  ;; === Global section ===
                  (let* ((needs-bump-ptr (or needs-bytevector wit-needs-memory))
                         (bump-ptr-idx -1)
                         (sym-table-gidx -1)
                         (escape-id-gidx -1)
                         (escape-val-gidx -1))
                    (when (or (> nglobals 0) needs-bump-ptr needs-symbol needs-callcc)
                      (wbuf-reset! sec)
                      (wbuf-u32! sec (+ nglobals
                                        (if needs-bump-ptr 1 0)
                                        (if needs-symbol 1 0)
                                        (if needs-callcc 2 0)))
                      (let loop ((i 0))
                        (when (< i nglobals)
                          (wbuf-byte! sec HT-EQ) (wbuf-byte! sec #x01)
                          (wbuf-byte! sec OP-REF-NULL) (wbuf-byte! sec HT-EQ) (wbuf-byte! sec OP-END)
                          (loop (+ i 1))))
                      (when needs-bump-ptr
                        (set! bump-ptr-idx nglobals)
                        (wbuf-byte! sec TYPE-I32) (wbuf-byte! sec #x01)
                        (wbuf-byte! sec OP-I32-CONST) (wbuf-i32! sec 4096) (wbuf-byte! sec OP-END))
                      (when needs-symbol
                        (set! sym-table-gidx (+ nglobals (if needs-bump-ptr 1 0)))
                        (wbuf-byte! sec HT-EQ) (wbuf-byte! sec #x01)
                        (wbuf-byte! sec OP-REF-NULL) (wbuf-byte! sec HT-EQ) (wbuf-byte! sec OP-END))
                      (when needs-callcc
                        ;; escape-id: i32, mutable, initialized to -1 (no active escape)
                        (set! escape-id-gidx (+ nglobals (if needs-bump-ptr 1 0) (if needs-symbol 1 0)))
                        (set! escape-val-gidx (+ escape-id-gidx 1))
                        (wbuf-byte! sec TYPE-I32) (wbuf-byte! sec #x01)
                        (wbuf-byte! sec OP-I32-CONST) (wbuf-i32! sec -1) (wbuf-byte! sec OP-END)
                        ;; escape-val: eqref, mutable, initialized to null
                        (wbuf-byte! sec HT-EQ) (wbuf-byte! sec #x01)
                        (wbuf-byte! sec OP-REF-NULL) (wbuf-byte! sec HT-EQ) (wbuf-byte! sec OP-END))
                      (wbuf-section! out SEC-GLOBAL sec))

                    ;; === Tag section (exception handling) ===
                    ;; Must appear after global section (per WASM exception handling spec).
                    (when needs-callcc
                      (wbuf-reset! sec)
                      (wbuf-u32! sec 1)          ;; 1 tag
                      (wbuf-byte! sec #x00)       ;; tag attribute = 0 (exception)
                      (wbuf-u32! sec TY-VOID-VOID) ;; type = (func) — no params, no results
                      (wbuf-section! out SEC-TAG sec))

                    ;; === Export section ===
                    (wbuf-reset! sec)
                    (if *wit-world*
                        ;; WIT: export memory + _start + externals + wit shims
                        (begin
                          (wbuf-u32! sec (+ 2 nexternals n-wit-shims))
                          (wbuf-name! sec "memory")
                          (wbuf-byte! sec #x02) (wbuf-u32! sec 0)
                          (wbuf-name! sec "_start")
                          (wbuf-byte! sec #x00) (wbuf-u32! sec fn-start)
                          (for-each
                           (lambda (ext)
                             (wbuf-name! sec (ext-export-name ext))
                             (wbuf-byte! sec #x00)
                             (wbuf-u32! sec (ext-wrapper-func-idx ext)))
                           externals)
                          (for-each
                           (lambda (ws)
                             (wbuf-name! sec (vector-ref ws 0))
                             (wbuf-byte! sec #x00)
                             (wbuf-u32! sec (vector-ref ws 4)))
                           wit-shim-info))
                        ;; WASI: export only "run"
                        (begin
                          (wbuf-u32! sec 1)
                          (wbuf-name! sec "run")
                          (wbuf-byte! sec #x00)
                          (wbuf-u32! sec (+ fn-start nbuiltins nfuncs n-wit-shims))))
                    (wbuf-section! out SEC-EXPORT sec)

                    ;; === Element section ===
                    (when (> nfuncs 0)
                      (wbuf-reset! sec)
                      (wbuf-u32! sec 1)
                      (wbuf-u32! sec 0)
                      (wbuf-byte! sec OP-I32-CONST) (wbuf-i32! sec 0) (wbuf-byte! sec OP-END)
                      (wbuf-u32! sec nfuncs)
                      (for-each (lambda (uf) (wbuf-u32! sec (uf-func-idx uf))) funcs)
                      (wbuf-section! out SEC-ELEMENT sec))

                    ;; === Code section ===

                    (when *profile* (phase-time "  codegen/sections" codegen-t0))

                    ;; Build hash tables for O(1) lookups (once for entire compilation)
                    (init-ctx-hashes! globals funcs boxed-vars)
                    (init-codegen-ops!)

                    ;; Build index vectors for wrapper/lambda lookup by func-idx
                    (let* ((nfuncs-total (length funcs))
                           (wrapper-vec (make-vector nfuncs-total #f))
                           (lambda-vec (make-vector nfuncs-total #f))
                           (ext-vec (make-vector nfuncs-total #f)))
                      (for-each (lambda (w)
                                  (let ((i (- (wi-wrapper-fidx w) fn-user-start)))
                                    (when (and (>= i 0) (< i nfuncs-total))
                                      (vector-set! wrapper-vec i w))))
                                wrappers)
                      (for-each (lambda (l)
                                  (let ((i (- (ll-func-idx l) fn-user-start)))
                                    (when (and (>= i 0) (< i nfuncs-total))
                                      (vector-set! lambda-vec i l))))
                                lambdas)
                      (for-each (lambda (e)
                                  (let ((i (- (ext-wrapper-func-idx e) fn-user-start)))
                                    (when (and (>= i 0) (< i nfuncs-total))
                                      (vector-set! ext-vec i e))))
                                externals)

                    ;; Build _start body
                    (let ((body (wbuf-make))
                          (start-ctx (make-ctx '() 0 globals funcs lambdas wrappers
                                               closure-arities nclosure-arity narity
                                               fn-display fn-newline-fn fn-eqv fn-equal
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
                                               ty-promise
                                               escape-cont-table-idx
                                               escape-id-gidx
                                               escape-val-gidx))
                          (ok #t))

                      (let ((nstart-locals (count-let-locals forms)))
                        (if (> nstart-locals 0)
                            (begin
                              (wbuf-u32! body 1)
                              (wbuf-u32! body nstart-locals)
                              (wbuf-byte! body HT-EQ))
                            (wbuf-u32! body 0)))

                      (let ((nforms (length forms)))
                        (let loop ((i 0) (fs forms))
                          (when (and ok (pair? fs))
                            (let ((form (car fs))
                                  (is-last (= i (- nforms 1))))
                              (cond
                                ((or (is-func-define? form) (is-external-define? form))
                                 (when is-last
                                   (emit-void! body)))
                                ((is-var-define? form)
                                 (set! ok (codegen-expr (caddr form) body start-ctx))
                                 (when ok
                                   (let ((gidx (ctx-global start-ctx (symbol->string (cadr form)))))
                                     (wbuf-byte! body OP-GLOBAL-SET) (wbuf-u32! body gidx))
                                   (when is-last
                                     (emit-void! body))))
                                (else
                                 (set! ok (codegen-expr form body start-ctx))
                                 (when (and ok (not is-last))
                                   (wbuf-byte! body OP-DROP))))
                            (loop (+ i 1) (cdr fs))))))

                      (if (not ok)
                          #f
                          (begin
                            (wbuf-byte! body OP-DROP)
                            (wbuf-byte! body OP-END)

                            (wbuf-reset! sec)
                            (wbuf-u32! sec (+ nbuiltins nfuncs n-wit-shims (if *wit-world* 0 1)))
                            (wbuf-func-body! sec body)

                            ;; === Builtin bodies (via C FFI) ===
                            (when *profile* (phase-time "  codegen/start-body" codegen-t0))
                            (when needs-display
                              (let ((rt-spec (make-rt-display fn-display
                                                 needs-flonum needs-rational needs-complex
                                                 needs-symbol)))
                                (compile-rt! sec rt-spec start-ctx)))
                            (when needs-write
                              (compile-rt! sec
                                (make-rt-write fn-write
                                               needs-flonum needs-rational needs-complex
                                               needs-symbol)
                                start-ctx))
                            (when needs-newline
                              (compile-rt! sec rt-newline-wasi start-ctx))
                            (when needs-eqv
                              (compile-rt! sec
                                (make-rt-eqv needs-flonum needs-rational needs-complex fn-eqv)
                                start-ctx))
                            (when needs-equal
                              (compile-rt! sec (make-rt-equal fn-equal) start-ctx))
                            (when needs-num-to-str
                              (compile-rt! sec rt-number-to-string start-ctx))
                            (when needs-str-to-num
                              (compile-rt! sec rt-string-to-number start-ctx))

                            ;; Numeric dispatch helpers
                            (when needs-dispatch
                              ;; 9 binop dispatch functions
                              (let ((binops (list
                                     (list '%i31-add '%f64-add #f #f)
                                     (list '%i31-sub '%f64-sub #f #f)
                                     (list '%i31-mul '%f64-mul #f #f)
                                     (list '%i31-div '%f64-div #f #t)
                                     (list '%i31-eq  '%f64-eq  #t #f)
                                     (list '%i31-lt  '%f64-lt  #t #f)
                                     (list '%i31-gt  '%f64-gt  #t #f)
                                     (list '%i31-le  '%f64-le  #t #f)
                                     (list '%i31-ge  '%f64-ge  #t #f))))
                                (for-each
                                 (lambda (spec)
                                   (compile-rt! sec
                                     (make-rt-numeric-binop
                                       (car spec) (cadr spec) (caddr spec) (cadddr spec)
                                       fn-flonum-start fn-gcd fn-make-complex
                                       needs-flonum needs-rational needs-complex)
                                     start-ctx))
                                 binops))
                              ;; exact->inexact
                              (compile-rt! sec
                                (make-rt-exact-to-inexact fn-flonum-start fn-make-complex
                                                          needs-flonum needs-rational needs-complex)
                                start-ctx)
                              ;; inexact->exact
                              (compile-rt! sec
                                (make-rt-inexact-to-exact fn-flonum-start fn-make-complex
                                                          needs-flonum needs-rational needs-complex)
                                start-ctx)
                              ;; floor, ceiling, truncate, round
                              (for-each
                               (lambda (round-op)
                                 (compile-rt! sec
                                   (make-rt-round-op round-op
                                                      needs-flonum needs-rational needs-complex)
                                   start-ctx))
                               (list '%f64-floor '%f64-ceil '%f64-trunc '%f64-nearest)))

                            ;; GCD helper
                            (when needs-rational
                              (compile-rt! sec rt-gcd-helper start-ctx))
                            ;; make_complex
                            (when needs-complex
                              (compile-rt! sec rt-make-complex start-ctx))

                            ;; Math functions
                            (when needs-math
                              (let ((nf needs-flonum) (nr needs-rational))
                                (for-each
                                 (lambda (maker)
                                   (compile-rt! sec (maker nf nr) start-ctx))
                                 (list make-rt-math-sqrt make-rt-math-exp make-rt-math-log
                                       make-rt-math-sin make-rt-math-cos make-rt-math-tan
                                       make-rt-math-asin make-rt-math-acos make-rt-math-atan)))
                              ;; atan2
                              (compile-rt! sec
                                (make-rt-math-atan2 fn-math-start needs-flonum needs-rational)
                                start-ctx)
                              ;; expt
                              (compile-rt! sec
                                (make-rt-math-expt fn-flonum-start fn-math-start needs-flonum needs-rational)
                                start-ctx))

                            ;; command-line
                            (when needs-command-line
                              (compile-rt! sec rt-command-line start-ctx))
                            ;; string-append
                            (when needs-string-append
                              (compile-rt! sec rt-string-append start-ctx))
                            ;; bytevector helpers
                            (when needs-bytevector
                              (compile-rt! sec rt-linear-alloc start-ctx)
                              (compile-rt! sec (make-rt-bv-alloc fn-linear-alloc) start-ctx)
                              (compile-rt! sec rt-bv-copy-str start-ctx)
                              (compile-rt! sec rt-ptr-to-str start-ctx)
                              (compile-rt! sec (make-rt-bv-alloc-fill fn-linear-alloc) start-ctx))
                            (when needs-bv-copy
                              (compile-rt! sec (make-rt-bv-copy-range fn-linear-alloc) start-ctx)
                              (compile-rt! sec (make-rt-bv-copy fn-bv-copy-range) start-ctx)
                              (compile-rt! sec (make-rt-bv-copy-from fn-bv-copy-range) start-ctx))
                            (when needs-bv-append
                              (compile-rt! sec (make-rt-bv-append fn-linear-alloc) start-ctx))
                            (when needs-utf8-string
                              (compile-rt! sec (make-rt-utf8-to-str-range fn-ptr-to-str) start-ctx)
                              (compile-rt! sec (make-rt-utf8-to-str fn-utf8-to-str-range) start-ctx)
                              (compile-rt! sec (make-rt-utf8-to-str-from fn-utf8-to-str-range) start-ctx)
                              (compile-rt! sec (make-rt-str-to-utf8-range fn-linear-alloc) start-ctx)
                              (compile-rt! sec (make-rt-str-to-utf8 fn-str-to-utf8-range) start-ctx)
                              (compile-rt! sec (make-rt-str-to-utf8-from fn-str-to-utf8-range) start-ctx))
                            ;; file I/O helpers
                            (when needs-file-io
                              (compile-rt! sec rt-open-input-file start-ctx)
                              (compile-rt! sec rt-open-output-file start-ctx)
                              (compile-rt! sec rt-close-port start-ctx)
                              (compile-rt! sec rt-read-char start-ctx)
                              (compile-rt! sec rt-peek-char start-ctx)
                              (compile-rt! sec rt-write-char start-ctx)
                              (compile-rt! sec rt-open-input-string start-ctx))
                            ;; string operations
                            (when needs-string-ops
                              (compile-rt! sec rt-substring start-ctx)
                              (compile-rt! sec rt-string-copy start-ctx)
                              (compile-rt! sec rt-string-to-list start-ctx)
                              (compile-rt! sec rt-list-to-string start-ctx)
                              (compile-rt! sec rt-string-eq start-ctx)
                              (compile-rt! sec rt-string-lt start-ctx)
                              (compile-rt! sec rt-string-ci-eq start-ctx)
                              (compile-rt! sec rt-string-ci-lt start-ctx)
                              (compile-rt! sec rt-string-fill start-ctx))
                            ;; read
                            (when needs-read
                              (compile-rt! sec
                                (make-rt-read fn-read fn-read-list fn-read-string
                                              fn-peek-char-fn fn-read-char-fn)
                                start-ctx)
                              (compile-rt! sec
                                (make-rt-read-list fn-read fn-read-list
                                                   fn-peek-char-fn fn-read-char-fn)
                                start-ctx)
                              (compile-rt! sec
                                (make-rt-read-string fn-read-char-fn)
                                start-ctx))
                            ;; vector helpers
                            (when needs-vector
                              (compile-rt! sec rt-vector-copy start-ctx)
                              (compile-rt! sec rt-list-to-vector start-ctx)
                              (compile-rt! sec rt-vector-to-list start-ctx)
                              (compile-rt! sec rt-vector-fill start-ctx))

                            ;; symbol intern function (hand-written WASM)
                            (when needs-symbol
                              (let ((ibody (wbuf-make)))
                                ;; Locals: 4 eqref, 2 i32
                                (wbuf-u32! ibody 2)       ;; 2 local groups
                                (wbuf-u32! ibody 4)       ;; 4 eqref
                                (wbuf-byte! ibody HT-EQ)
                                (wbuf-u32! ibody 2)       ;; 2 i32
                                (wbuf-byte! ibody TYPE-I32)
                                ;; slot 0=param(eqref) 1-4=eqref locals 5-6=i32 locals
                                ;; Get input string length
                                (wbuf-byte! ibody OP-LOCAL-GET) (wbuf-u32! ibody 0)
                                (wbuf-byte! ibody OP-GC-PREFIX) (wbuf-byte! ibody GC-REF-CAST) (wbuf-u32! ibody TY-STRING)
                                (wbuf-byte! ibody OP-GC-PREFIX) (wbuf-byte! ibody GC-ARRAY-LEN)
                                (wbuf-byte! ibody OP-LOCAL-SET) (wbuf-u32! ibody 6)
                                ;; Load intern table
                                (wbuf-byte! ibody OP-GLOBAL-GET) (wbuf-u32! ibody sym-table-gidx)
                                (wbuf-byte! ibody OP-LOCAL-SET) (wbuf-u32! ibody 1)
                                ;; block $done (result eqref)
                                (wbuf-byte! ibody OP-BLOCK) (wbuf-byte! ibody HT-EQ)
                                ;; block $not_found (void)
                                (wbuf-byte! ibody OP-BLOCK) (wbuf-byte! ibody #x40)
                                ;; loop $search (void)
                                (wbuf-byte! ibody OP-LOOP) (wbuf-byte! ibody #x40)
                                ;; Check if null
                                (wbuf-byte! ibody OP-LOCAL-GET) (wbuf-u32! ibody 1)
                                (wbuf-byte! ibody OP-REF-IS-NULL)
                                (wbuf-byte! ibody OP-BR-IF) (wbuf-u32! ibody 1)  ;; → $not_found
                                ;; Cast to pair, save pair ref
                                (wbuf-byte! ibody OP-LOCAL-GET) (wbuf-u32! ibody 1)
                                (wbuf-byte! ibody OP-GC-PREFIX) (wbuf-byte! ibody GC-REF-CAST) (wbuf-u32! ibody TY-PAIR)
                                (wbuf-byte! ibody OP-LOCAL-TEE) (wbuf-u32! ibody 2)
                                ;; Get car = the symbol (re-cast after tee since stack is now eqref)
                                (wbuf-byte! ibody OP-GC-PREFIX) (wbuf-byte! ibody GC-REF-CAST) (wbuf-u32! ibody TY-PAIR)
                                (wbuf-byte! ibody OP-GC-PREFIX) (wbuf-byte! ibody GC-STRUCT-GET) (wbuf-u32! ibody TY-PAIR) (wbuf-u32! ibody 0)
                                (wbuf-byte! ibody OP-GC-PREFIX) (wbuf-byte! ibody GC-REF-CAST) (wbuf-u32! ibody ty-symbol)
                                (wbuf-byte! ibody OP-LOCAL-TEE) (wbuf-u32! ibody 3)
                                ;; Get symbol's string (re-cast after tee)
                                (wbuf-byte! ibody OP-GC-PREFIX) (wbuf-byte! ibody GC-REF-CAST) (wbuf-u32! ibody ty-symbol)
                                (wbuf-byte! ibody OP-GC-PREFIX) (wbuf-byte! ibody GC-STRUCT-GET) (wbuf-u32! ibody ty-symbol) (wbuf-u32! ibody 0)
                                (wbuf-byte! ibody OP-LOCAL-TEE) (wbuf-u32! ibody 4)
                                ;; Compare lengths
                                (wbuf-byte! ibody OP-GC-PREFIX) (wbuf-byte! ibody GC-REF-CAST) (wbuf-u32! ibody TY-STRING)
                                (wbuf-byte! ibody OP-GC-PREFIX) (wbuf-byte! ibody GC-ARRAY-LEN)
                                (wbuf-byte! ibody OP-LOCAL-GET) (wbuf-u32! ibody 6)
                                (wbuf-byte! ibody OP-I32-NE)
                                ;; If lengths differ, advance cdr and loop
                                (wbuf-byte! ibody OP-IF) (wbuf-byte! ibody #x40)
                                (wbuf-byte! ibody OP-LOCAL-GET) (wbuf-u32! ibody 2)
                                (wbuf-byte! ibody OP-GC-PREFIX) (wbuf-byte! ibody GC-REF-CAST) (wbuf-u32! ibody TY-PAIR)
                                (wbuf-byte! ibody OP-GC-PREFIX) (wbuf-byte! ibody GC-STRUCT-GET) (wbuf-u32! ibody TY-PAIR) (wbuf-u32! ibody 1)
                                (wbuf-byte! ibody OP-LOCAL-SET) (wbuf-u32! ibody 1)
                                (wbuf-byte! ibody OP-BR) (wbuf-u32! ibody 1) ;; → $search (if=0, search=1)
                                (wbuf-byte! ibody OP-END) ;; end if
                                ;; Lengths match — compare chars
                                (wbuf-byte! ibody OP-I32-CONST) (wbuf-i32! ibody 0)
                                (wbuf-byte! ibody OP-LOCAL-SET) (wbuf-u32! ibody 5)
                                ;; block $mismatch (void)
                                (wbuf-byte! ibody OP-BLOCK) (wbuf-byte! ibody #x40)
                                ;; loop $char_loop (void)
                                (wbuf-byte! ibody OP-LOOP) (wbuf-byte! ibody #x40)
                                ;; If index >= length, all chars match → found!
                                (wbuf-byte! ibody OP-LOCAL-GET) (wbuf-u32! ibody 5)
                                (wbuf-byte! ibody OP-LOCAL-GET) (wbuf-u32! ibody 6)
                                (wbuf-byte! ibody OP-I32-GE-U)
                                (wbuf-byte! ibody OP-IF) (wbuf-byte! ibody #x40)
                                (wbuf-byte! ibody OP-LOCAL-GET) (wbuf-u32! ibody 3) ;; the symbol
                                (wbuf-byte! ibody OP-BR) (wbuf-u32! ibody 5) ;; → $done (if=0,loop=1,blk=2,search=3,notfound=4,done=5)
                                (wbuf-byte! ibody OP-END) ;; end if
                                ;; Compare input char vs existing char
                                (wbuf-byte! ibody OP-LOCAL-GET) (wbuf-u32! ibody 0) ;; input string
                                (wbuf-byte! ibody OP-GC-PREFIX) (wbuf-byte! ibody GC-REF-CAST) (wbuf-u32! ibody TY-STRING)
                                (wbuf-byte! ibody OP-LOCAL-GET) (wbuf-u32! ibody 5) ;; index
                                (wbuf-byte! ibody OP-GC-PREFIX) (wbuf-byte! ibody GC-ARRAY-GET-U) (wbuf-u32! ibody TY-STRING)
                                (wbuf-byte! ibody OP-LOCAL-GET) (wbuf-u32! ibody 4) ;; existing string
                                (wbuf-byte! ibody OP-GC-PREFIX) (wbuf-byte! ibody GC-REF-CAST) (wbuf-u32! ibody TY-STRING)
                                (wbuf-byte! ibody OP-LOCAL-GET) (wbuf-u32! ibody 5) ;; index
                                (wbuf-byte! ibody OP-GC-PREFIX) (wbuf-byte! ibody GC-ARRAY-GET-U) (wbuf-u32! ibody TY-STRING)
                                (wbuf-byte! ibody OP-I32-NE)
                                (wbuf-byte! ibody OP-BR-IF) (wbuf-u32! ibody 1) ;; → $mismatch (loop=0,blk=1)
                                ;; Increment index and continue
                                (wbuf-byte! ibody OP-LOCAL-GET) (wbuf-u32! ibody 5)
                                (wbuf-byte! ibody OP-I32-CONST) (wbuf-i32! ibody 1)
                                (wbuf-byte! ibody OP-I32-ADD)
                                (wbuf-byte! ibody OP-LOCAL-SET) (wbuf-u32! ibody 5)
                                (wbuf-byte! ibody OP-BR) (wbuf-u32! ibody 0) ;; → $char_loop
                                (wbuf-byte! ibody OP-END) ;; end loop $char_loop
                                (wbuf-byte! ibody OP-END) ;; end block $mismatch
                                ;; Chars didn't match, advance to cdr
                                (wbuf-byte! ibody OP-LOCAL-GET) (wbuf-u32! ibody 2)
                                (wbuf-byte! ibody OP-GC-PREFIX) (wbuf-byte! ibody GC-REF-CAST) (wbuf-u32! ibody TY-PAIR)
                                (wbuf-byte! ibody OP-GC-PREFIX) (wbuf-byte! ibody GC-STRUCT-GET) (wbuf-u32! ibody TY-PAIR) (wbuf-u32! ibody 1)
                                (wbuf-byte! ibody OP-LOCAL-SET) (wbuf-u32! ibody 1)
                                (wbuf-byte! ibody OP-BR) (wbuf-u32! ibody 0) ;; → $search
                                (wbuf-byte! ibody OP-END) ;; end loop $search
                                (wbuf-byte! ibody OP-END) ;; end block $not_found
                                ;; Not found — create new symbol
                                (wbuf-byte! ibody OP-LOCAL-GET) (wbuf-u32! ibody 0)
                                (wbuf-byte! ibody OP-GC-PREFIX) (wbuf-byte! ibody GC-REF-CAST-NULL) (wbuf-u32! ibody TY-STRING)
                                (wbuf-byte! ibody OP-GC-PREFIX) (wbuf-byte! ibody GC-STRUCT-NEW) (wbuf-u32! ibody ty-symbol)
                                (wbuf-byte! ibody OP-LOCAL-SET) (wbuf-u32! ibody 3)
                                ;; Prepend to table: cons(symbol, old_table)
                                (wbuf-byte! ibody OP-LOCAL-GET) (wbuf-u32! ibody 3)
                                (wbuf-byte! ibody OP-GLOBAL-GET) (wbuf-u32! ibody sym-table-gidx)
                                (wbuf-byte! ibody OP-GC-PREFIX) (wbuf-byte! ibody GC-STRUCT-NEW) (wbuf-u32! ibody TY-PAIR)
                                (wbuf-byte! ibody OP-GLOBAL-SET) (wbuf-u32! ibody sym-table-gidx)
                                ;; Return new symbol
                                (wbuf-byte! ibody OP-LOCAL-GET) (wbuf-u32! ibody 3)
                                (wbuf-byte! ibody OP-END) ;; end block $done
                                (wbuf-byte! ibody OP-END) ;; end function
                                (wbuf-func-body! sec ibody)))

                            ;; apply function (hand-written WASM)
                            (when needs-apply
                              (let ((abody (wbuf-make))
                                    (K nclosure-arity)
                                    (sorted-arities (let sort-arities ((cas closure-arities) (acc '()))
                                                      (if (null? cas) acc
                                                          (sort-arities (cdr cas)
                                                            (let insert ((n (car cas)) (rest acc) (prev '()))
                                                              (cond
                                                                ((null? rest) (append (reverse prev) (list n)))
                                                                ((<= n (car rest))
                                                                 (append (reverse prev) (cons n rest)))
                                                                (else (insert n (cdr rest) (cons (car rest) prev))))))))))
                                (let ((max-arity (if (null? sorted-arities) 0
                                                     (let loop ((cas sorted-arities) (mx 0))
                                                       (if (null? cas) mx
                                                           (loop (cdr cas) (if (> (car cas) mx) (car cas) mx)))))))
                                  ;; Locals: 1 eqref (cursor), 1 i32 (length)
                                  (wbuf-u32! abody 2)
                                  (wbuf-u32! abody 1) (wbuf-byte! abody HT-EQ)
                                  (wbuf-u32! abody 1) (wbuf-byte! abody TYPE-I32)
                                  ;; params: 0=proc(eqref), 1=args(eqref)
                                  ;; locals: 2=cursor(eqref), 3=len(i32)
                                  ;; Count list length
                                  (wbuf-byte! abody OP-I32-CONST) (wbuf-i32! abody 0)
                                  (wbuf-byte! abody OP-LOCAL-SET) (wbuf-u32! abody 3)
                                  (wbuf-byte! abody OP-LOCAL-GET) (wbuf-u32! abody 1)
                                  (wbuf-byte! abody OP-LOCAL-SET) (wbuf-u32! abody 2)
                                  (wbuf-byte! abody OP-BLOCK) (wbuf-byte! abody #x40)
                                  (wbuf-byte! abody OP-LOOP) (wbuf-byte! abody #x40)
                                  (wbuf-byte! abody OP-LOCAL-GET) (wbuf-u32! abody 2)
                                  (wbuf-byte! abody OP-REF-IS-NULL)
                                  (wbuf-byte! abody OP-BR-IF) (wbuf-u32! abody 1)
                                  (wbuf-byte! abody OP-LOCAL-GET) (wbuf-u32! abody 2)
                                  (wbuf-byte! abody OP-GC-PREFIX) (wbuf-byte! abody GC-REF-CAST) (wbuf-u32! abody TY-PAIR)
                                  (wbuf-byte! abody OP-GC-PREFIX) (wbuf-byte! abody GC-STRUCT-GET) (wbuf-u32! abody TY-PAIR) (wbuf-u32! abody 1)
                                  (wbuf-byte! abody OP-LOCAL-SET) (wbuf-u32! abody 2)
                                  (wbuf-byte! abody OP-LOCAL-GET) (wbuf-u32! abody 3)
                                  (wbuf-byte! abody OP-I32-CONST) (wbuf-i32! abody 1)
                                  (wbuf-byte! abody OP-I32-ADD)
                                  (wbuf-byte! abody OP-LOCAL-SET) (wbuf-u32! abody 3)
                                  (wbuf-byte! abody OP-BR) (wbuf-u32! abody 0)
                                  (wbuf-byte! abody OP-END) ;; end loop
                                  (wbuf-byte! abody OP-END) ;; end block

                                  ;; Dispatch: block structure
                                  ;; block $result (result eqref)
                                  ;;   block $trap
                                  ;;     block $aK-1 ... block $a0
                                  ;;       br_table ...
                                  ;;     end $a0  → code for sorted[0], br $result
                                  ;;     ...
                                  ;;   end $trap  → unreachable
                                  ;; end $result
                                  (wbuf-byte! abody OP-BLOCK) (wbuf-byte! abody HT-EQ) ;; $result
                                  (wbuf-byte! abody OP-BLOCK) (wbuf-byte! abody #x40)  ;; $trap
                                  (let loop ((i 0))
                                    (when (< i K)
                                      (wbuf-byte! abody OP-BLOCK) (wbuf-byte! abody #x40)
                                      (loop (+ i 1))))

                                  ;; br_table: len entries for 0..max_arity, plus default
                                  (wbuf-byte! abody OP-LOCAL-GET) (wbuf-u32! abody 3)
                                  (wbuf-byte! abody OP-BR-TABLE)
                                  (wbuf-u32! abody (+ max-arity 1)) ;; number of labels (0..max-arity)
                                  (let loop ((v 0))
                                    (when (<= v max-arity)
                                      (let find ((j 0) (cas sorted-arities))
                                        (cond
                                          ((null? cas)
                                           (wbuf-u32! abody K)) ;; → $trap
                                          ((= (car cas) v)
                                           (wbuf-u32! abody j)) ;; → $a{j}
                                          (else (find (+ j 1) (cdr cas)))))
                                      (loop (+ v 1))))
                                  (wbuf-u32! abody K) ;; default → $trap

                                  ;; Code for each arity
                                  (let loop ((i 0) (cas sorted-arities))
                                    (when (pair? cas)
                                      (let ((arity (car cas)))
                                        (wbuf-byte! abody OP-END) ;; end block $a{i}
                                        ;; Push env: proc.env
                                        (wbuf-byte! abody OP-LOCAL-GET) (wbuf-u32! abody 0)
                                        (wbuf-byte! abody OP-GC-PREFIX) (wbuf-byte! abody GC-REF-CAST) (wbuf-u32! abody TY-CLOSURE)
                                        (wbuf-byte! abody OP-GC-PREFIX) (wbuf-byte! abody GC-STRUCT-GET) (wbuf-u32! abody TY-CLOSURE) (wbuf-u32! abody 1)
                                        ;; Push args: car(cdr^j(args)) for j=0..arity-1
                                        (let aloop ((j 0))
                                          (when (< j arity)
                                            (wbuf-byte! abody OP-LOCAL-GET) (wbuf-u32! abody 1)
                                            (let cdr-loop ((c 0))
                                              (when (< c j)
                                                (wbuf-byte! abody OP-GC-PREFIX) (wbuf-byte! abody GC-REF-CAST) (wbuf-u32! abody TY-PAIR)
                                                (wbuf-byte! abody OP-GC-PREFIX) (wbuf-byte! abody GC-STRUCT-GET) (wbuf-u32! abody TY-PAIR) (wbuf-u32! abody 1)
                                                (cdr-loop (+ c 1))))
                                            (wbuf-byte! abody OP-GC-PREFIX) (wbuf-byte! abody GC-REF-CAST) (wbuf-u32! abody TY-PAIR)
                                            (wbuf-byte! abody OP-GC-PREFIX) (wbuf-byte! abody GC-STRUCT-GET) (wbuf-u32! abody TY-PAIR) (wbuf-u32! abody 0)
                                            (aloop (+ j 1))))
                                        ;; Push code index: proc.code
                                        (wbuf-byte! abody OP-LOCAL-GET) (wbuf-u32! abody 0)
                                        (wbuf-byte! abody OP-GC-PREFIX) (wbuf-byte! abody GC-REF-CAST) (wbuf-u32! abody TY-CLOSURE)
                                        (wbuf-byte! abody OP-GC-PREFIX) (wbuf-byte! abody GC-STRUCT-GET) (wbuf-u32! abody TY-CLOSURE) (wbuf-u32! abody 0)
                                        ;; Find closure type for this arity
                                        (let ((ctidx (let find-ct ((j 0) (all-cas closure-arities))
                                                       (cond ((null? all-cas) -1)
                                                             ((= (car all-cas) arity) (+ ty-user-start narity j))
                                                             (else (find-ct (+ j 1) (cdr all-cas)))))))
                                          (wbuf-byte! abody OP-CALL-INDIRECT)
                                          (wbuf-u32! abody ctidx)
                                          (wbuf-u32! abody 0))
                                        ;; br $result
                                        (wbuf-byte! abody OP-BR) (wbuf-u32! abody (- K i)))
                                      (loop (+ i 1) (cdr cas))))

                                  (wbuf-byte! abody OP-END)  ;; end $trap
                                  (wbuf-byte! abody #x00)    ;; unreachable
                                  (wbuf-byte! abody OP-END)  ;; end $result
                                  (wbuf-byte! abody OP-END)  ;; end function
                                  (wbuf-func-body! sec abody))))

                            ;; file-exists?
                            (when needs-file-exists
                              (compile-rt! sec rt-file-exists start-ctx))
                            ;; get-environment-variable / get-environment-variables
                            (when needs-get-env
                              (compile-rt! sec rt-get-env-var start-ctx)
                              (compile-rt! sec rt-get-env-vars start-ctx))

                            ;; === User function bodies ===
                            (when *profile* (phase-time "  codegen/builtins" codegen-t0))
                            (let ((user-ok #t))
                              (for-each
                               (lambda (uf)
                                 (when user-ok
                                   (let ((ubody (wbuf-make))
                                         (uf-t0 (if *profile* (current-milliseconds) 0)))

                                     ;; Check if this is a closure wrapper (O(1) vector lookup)
                                     (let ((fidx-offset (- (uf-func-idx uf) fn-user-start))
                                           (wrapped (let ((i (- (uf-func-idx uf) fn-user-start)))
                                                      (and (>= i 0) (< i nfuncs-total)
                                                           (vector-ref wrapper-vec i)))))
                                       (if wrapped
                                           ;; Wrapper body
                                           (let ((np (wi-nparams wrapped)))
                                             (wbuf-u32! ubody 0)
                                             (let loop ((j 0))
                                               (when (< j np)
                                                 (wbuf-byte! ubody OP-LOCAL-GET) (wbuf-u32! ubody (+ 1 j))
                                                 (loop (+ j 1))))
                                             (wbuf-byte! ubody OP-CALL) (wbuf-u32! ubody (wi-orig-fidx wrapped))
                                             (wbuf-byte! ubody OP-END)
                                             (wbuf-func-body! sec ubody))

                                           ;; Check if external wrapper (O(1) vector lookup)
                                           (let ((ext-match (and (>= fidx-offset 0) (< fidx-offset nfuncs-total)
                                                                 (vector-ref ext-vec fidx-offset))))
                                             (if ext-match
                                                 ;; External wrapper body
                                                 (begin
                                                   (wbuf-u32! ubody 0)
                                                   (let loop ((j 0) (pts (ext-param-types ext-match)))
                                                     (when (pair? pts)
                                                       (wbuf-byte! ubody OP-LOCAL-GET) (wbuf-u32! ubody j)
                                                       (cond
                                                         ((= (car pts) TYPE-I32)
                                                          (emit-box-fixnum! ubody))
                                                         ((= (car pts) TYPE-F64)
                                                          (emit-box-f64! ubody ty-flonum)))
                                                       (loop (+ j 1) (cdr pts))))
                                                   (wbuf-byte! ubody OP-CALL)
                                                   (wbuf-u32! ubody (ext-internal-func-idx ext-match))
                                                   (cond
                                                     ((= (ext-return-type ext-match) 0)
                                                      (wbuf-byte! ubody OP-DROP))
                                                     ((= (ext-return-type ext-match) TYPE-I32)
                                                      (emit-unbox-fixnum! ubody))
                                                     ((= (ext-return-type ext-match) TYPE-F64)
                                                      (emit-unbox-f64! ubody ty-flonum)))
                                                   (wbuf-byte! ubody OP-END)
                                                   (wbuf-func-body! sec ubody))

                                                 ;; Check if escape_cont (body-forms = #f, no WIT match)
                                                 (if (and needs-callcc
                                                          (not (uf-body-forms uf))
                                                          (= (uf-func-idx uf) (+ fn-user-start escape-cont-table-idx)))
                                                     ;; escape_cont body: (env: ref$env, k: eqref) -> eqref
                                                     ;; Stores k in global escape-val, id from env[0] in escape-id, then throws.
                                                     (begin
                                                       (wbuf-u32! ubody 0) ;; no extra locals
                                                       ;; Set global escape-val = local[1] (k = the return value)
                                                       (wbuf-byte! ubody OP-LOCAL-GET) (wbuf-u32! ubody 1)
                                                       (wbuf-byte! ubody OP-GLOBAL-SET) (wbuf-u32! ubody escape-val-gidx)
                                                       ;; Set global escape-id = env[0] unboxed (local[0] = env array)
                                                       (wbuf-byte! ubody OP-LOCAL-GET) (wbuf-u32! ubody 0)
                                                       (wbuf-byte! ubody OP-I32-CONST) (wbuf-i32! ubody 0)
                                                       (wbuf-byte! ubody OP-GC-PREFIX) (wbuf-byte! ubody GC-ARRAY-GET)
                                                       (wbuf-u32! ubody TY-ENV)
                                                       (wbuf-byte! ubody OP-GC-PREFIX) (wbuf-byte! ubody GC-REF-CAST)
                                                       (wbuf-byte! ubody HT-I31)
                                                       (wbuf-byte! ubody OP-GC-PREFIX) (wbuf-byte! ubody GC-I31-GET-S)
                                                       (wbuf-byte! ubody OP-I32-CONST) (wbuf-i32! ubody 1)
                                                       (wbuf-byte! ubody OP-I32-SHR-S) ;; unbox fixnum (>> 1)
                                                       (wbuf-byte! ubody OP-GLOBAL-SET) (wbuf-u32! ubody escape-id-gidx)
                                                       ;; Throw the escape exception (tag index 0)
                                                       (wbuf-byte! ubody OP-THROW) (wbuf-u32! ubody 0)
                                                       (wbuf-byte! ubody OP-END)
                                                       (wbuf-func-body! sec ubody))

                                                 ;; Check if WIT import wrapper (body-forms = #f)
                                                 (if (not (uf-body-forms uf))
                                                     ;; WIT import wrapper body: unbox params, call import, box result
                                                     (let ((wi-match (let loop ((wis wit-import-info))
                                                                       (cond ((null? wis) #f)
                                                                             ((eq? (vector-ref (car wis) 1) uf) (car wis))
                                                                             (else (loop (cdr wis)))))))
                                                       (if wi-match
                                                           (let* ((wf (vector-ref wi-match 2))
                                                                  (params (wit-func-params wf))
                                                                  (result (wit-func-result wf))
                                                                  (import-idx (vector-ref wi-match 4))
                                                                  (nparams (length params))
                                                                  ;; Count string params for extra locals
                                                                  (nstring-params
                                                                   (let loop ((ps params) (n 0))
                                                                     (if (null? ps) n
                                                                         (loop (cdr ps)
                                                                               (if (wit-type-needs-memory? (cdar ps))
                                                                                   (+ n 1) n)))))
                                                                  ;; 3 i32 locals per string param: ptr, len, loop-i
                                                                  (nextra-locals (* nstring-params 3)))
                                                             ;; Locals declaration
                                                             (if (> nextra-locals 0)
                                                                 (begin
                                                                   (wbuf-u32! ubody 1)
                                                                   (wbuf-u32! ubody nextra-locals)
                                                                   (wbuf-byte! ubody TYPE-I32))
                                                                 (wbuf-u32! ubody 0))
                                                             ;; Unbox each eqref param to flat ABI type(s)
                                                             (let loop ((i 0) (ps params) (str-idx 0))
                                                               (when (pair? ps)
                                                                 (if (wit-type-needs-memory? (cdar ps))
                                                                     ;; String param: copy GC string to linear memory
                                                                     (let ((local-ptr (+ nparams (* str-idx 3)))
                                                                           (local-len (+ nparams (* str-idx 3) 1))
                                                                           (local-i   (+ nparams (* str-idx 3) 2)))
                                                                       ;; Get string length
                                                                       (wbuf-byte! ubody OP-LOCAL-GET) (wbuf-u32! ubody i)
                                                                       (wbuf-byte! ubody OP-GC-PREFIX) (wbuf-byte! ubody GC-REF-CAST)
                                                                       (wbuf-u32! ubody TY-STRING)
                                                                       (wbuf-byte! ubody OP-GC-PREFIX) (wbuf-byte! ubody GC-ARRAY-LEN)
                                                                       (wbuf-byte! ubody OP-LOCAL-SET) (wbuf-u32! ubody local-len)
                                                                       ;; Bump-allocate: ptr = bump; bump += len
                                                                       (wbuf-byte! ubody OP-GLOBAL-GET) (wbuf-u32! ubody bump-ptr-idx)
                                                                       (wbuf-byte! ubody OP-LOCAL-TEE) (wbuf-u32! ubody local-ptr)
                                                                       (wbuf-byte! ubody OP-LOCAL-GET) (wbuf-u32! ubody local-len)
                                                                       (wbuf-byte! ubody OP-I32-ADD)
                                                                       (wbuf-byte! ubody OP-GLOBAL-SET) (wbuf-u32! ubody bump-ptr-idx)
                                                                       ;; Copy loop: i=0; while(i<len) { mem[ptr+i] = str[i]; i++ }
                                                                       (wbuf-byte! ubody OP-I32-CONST) (wbuf-i32! ubody 0)
                                                                       (wbuf-byte! ubody OP-LOCAL-SET) (wbuf-u32! ubody local-i)
                                                                       (wbuf-byte! ubody OP-BLOCK) (wbuf-byte! ubody TYPE-VOID) ;; block $break
                                                                       (wbuf-byte! ubody OP-LOOP) (wbuf-byte! ubody TYPE-VOID)  ;; loop $cont
                                                                       ;; if i >= len, break
                                                                       (wbuf-byte! ubody OP-LOCAL-GET) (wbuf-u32! ubody local-i)
                                                                       (wbuf-byte! ubody OP-LOCAL-GET) (wbuf-u32! ubody local-len)
                                                                       (wbuf-byte! ubody OP-I32-GE-U)
                                                                       (wbuf-byte! ubody OP-BR-IF) (wbuf-u32! ubody 1) ;; br_if $break
                                                                       ;; mem[ptr+i] = array.get_u(str, i)
                                                                       (wbuf-byte! ubody OP-LOCAL-GET) (wbuf-u32! ubody local-ptr)
                                                                       (wbuf-byte! ubody OP-LOCAL-GET) (wbuf-u32! ubody local-i)
                                                                       (wbuf-byte! ubody OP-I32-ADD)
                                                                       (wbuf-byte! ubody OP-LOCAL-GET) (wbuf-u32! ubody i)
                                                                       (wbuf-byte! ubody OP-GC-PREFIX) (wbuf-byte! ubody GC-REF-CAST)
                                                                       (wbuf-u32! ubody TY-STRING)
                                                                       (wbuf-byte! ubody OP-LOCAL-GET) (wbuf-u32! ubody local-i)
                                                                       (wbuf-byte! ubody OP-GC-PREFIX) (wbuf-byte! ubody GC-ARRAY-GET-U)
                                                                       (wbuf-u32! ubody TY-STRING)
                                                                       (wbuf-byte! ubody OP-I32-STORE8) (wbuf-u32! ubody 0) (wbuf-u32! ubody 0)
                                                                       ;; i++
                                                                       (wbuf-byte! ubody OP-LOCAL-GET) (wbuf-u32! ubody local-i)
                                                                       (wbuf-byte! ubody OP-I32-CONST) (wbuf-i32! ubody 1)
                                                                       (wbuf-byte! ubody OP-I32-ADD)
                                                                       (wbuf-byte! ubody OP-LOCAL-SET) (wbuf-u32! ubody local-i)
                                                                       (wbuf-byte! ubody OP-BR) (wbuf-u32! ubody 0) ;; br $cont
                                                                       (wbuf-byte! ubody OP-END) ;; end loop
                                                                       (wbuf-byte! ubody OP-END) ;; end block
                                                                       ;; Push ptr, len for the flat call
                                                                       (wbuf-byte! ubody OP-LOCAL-GET) (wbuf-u32! ubody local-ptr)
                                                                       (wbuf-byte! ubody OP-LOCAL-GET) (wbuf-u32! ubody local-len)
                                                                       (loop (+ i 1) (cdr ps) (+ str-idx 1)))
                                                                     ;; Non-string param: unbox i31
                                                                     (begin
                                                                       (wbuf-byte! ubody OP-LOCAL-GET) (wbuf-u32! ubody i)
                                                                       (emit-unbox-fixnum! ubody)
                                                                       (loop (+ i 1) (cdr ps) str-idx)))))
                                                             ;; Call the lowered import
                                                             (wbuf-byte! ubody OP-CALL) (wbuf-u32! ubody import-idx)
                                                             ;; Box result back to eqref
                                                             (if result
                                                                 (if (wit-type-needs-memory? result)
                                                                     ;; String result: convert (ptr, len) to GC string
                                                                     ;; TODO: implement string return from imports
                                                                     (emit-box-fixnum! ubody)
                                                                     (emit-box-fixnum! ubody))
                                                                 (emit-void! ubody))
                                                             (wbuf-byte! ubody OP-END)
                                                             (wbuf-func-body! sec ubody))
                                                           (begin
                                                             (display "error: WIT import wrapper without matching import\n"
                                                                      (current-error-port))
                                                             (set! user-ok #f))))

                                                 ;; Regular user function (O(1) vector lookup)
                                                 (let ((ll (and (>= fidx-offset 0) (< fidx-offset nfuncs-total)
                                                                (vector-ref lambda-vec fidx-offset))))
                                                   (if (and ll (ll-is-closure ll))
                                                       ;; Closure function
                                                       (let* ((nfree-vars (ll-nfree ll))
                                                              (noriginal (ll-noriginal ll))
                                                              (nlet-locals (count-let-locals (uf-body-forms uf)))
                                                              (nextra (+ nfree-vars nlet-locals)))
                                                         (if (> nextra 0)
                                                             (begin
                                                               (wbuf-u32! ubody nextra)
                                                               (let loop ((j 0))
                                                                 (when (< j nextra)
                                                                   (wbuf-u32! ubody 1) (wbuf-byte! ubody HT-EQ)
                                                                   (loop (+ j 1)))))
                                                             (wbuf-u32! ubody 0))
                                                         ;; Unpack env
                                                         (let loop ((j 0))
                                                           (when (< j nfree-vars)
                                                             (wbuf-byte! ubody OP-LOCAL-GET) (wbuf-u32! ubody 0)
                                                             (wbuf-byte! ubody OP-I32-CONST) (wbuf-i32! ubody j)
                                                             (wbuf-byte! ubody OP-GC-PREFIX) (wbuf-byte! ubody GC-ARRAY-GET) (wbuf-u32! ubody TY-ENV)
                                                             (wbuf-byte! ubody OP-LOCAL-SET) (wbuf-u32! ubody (+ 1 noriginal j))
                                                             (loop (+ j 1))))
                                                         (let ((fn-ctx (ctx-with-locals start-ctx
                                                                                  (uf-param-names uf)
                                                                                  (+ 1 noriginal nfree-vars))))
                                                           ;; Set boxed-locals for closure: free vars that are in boxed-vars
                                                           (let ((boxed-fvs (filter (lambda (fv) (string-ht-has? (ctx-boxed-vars-hash fn-ctx) fv))
                                                                                    (ll-free-vars ll))))
                                                             (when (pair? boxed-fvs)
                                                               (set-boxed-locals! fn-ctx boxed-fvs)))
                                                           (set! user-ok (codegen-body/tail (uf-body-forms uf) ubody fn-ctx #t))
                                                           (when user-ok
                                                             (wbuf-byte! ubody OP-END)
                                                             (wbuf-func-body! sec ubody))))

                                                       ;; Regular function (no closure)
                                                       (let ((nlet-locals (count-let-locals (uf-body-forms uf))))
                                                         (if (> nlet-locals 0)
                                                             (begin
                                                               (wbuf-u32! ubody 1)
                                                               (wbuf-u32! ubody nlet-locals)
                                                               (wbuf-byte! ubody HT-EQ))
                                                             (wbuf-u32! ubody 0))
                                                         (let ((fn-ctx (ctx-with-locals start-ctx
                                                                                  (uf-param-names uf)
                                                                                  (uf-nparams uf))))
                                                           ;; Set boxed-locals for lifted lambda: free vars that are boxed
                                                           (when ll
                                                             (let ((boxed-fvs (filter (lambda (fv) (string-ht-has? (ctx-boxed-vars-hash fn-ctx) fv))
                                                                                      (ll-free-vars ll))))
                                                               (when (pair? boxed-fvs)
                                                                 (set-boxed-locals! fn-ctx boxed-fvs))))
                                                           (set! user-ok (codegen-body/tail (uf-body-forms uf) ubody fn-ctx #t))
                                                           (when user-ok
                                                             (wbuf-byte! ubody OP-END)
                                                             (wbuf-func-body! sec ubody)))))))))))
                                     (when *profile*
                                       (let ((elapsed (- (current-milliseconds) uf-t0)))
                                         (when (> elapsed 1000.0)
                                           (display "    func " (current-error-port))
                                           (display (if (uf-name uf) (uf-name uf) "(lambda)") (current-error-port))
                                           (display ": " (current-error-port))
                                           (display elapsed (current-error-port))
                                           (display "ms\n" (current-error-port))))))))
                               funcs)

                              (when *profile* (phase-time "  codegen/user-funcs" codegen-t0))
                              (if (not user-ok)
                                  #f
                                  (begin
                                    ;; WIT export shim function bodies
                                    (for-each
                                     (lambda (ws)
                                       (let* ((uf (vector-ref ws 1))
                                              (wf (vector-ref ws 2))
                                              (params (wit-func-params wf))
                                              (result (wit-func-result wf))
                                              (user-abs-idx (uf-func-idx uf))
                                              (sbody (wbuf-make))
                                              ;; Count flat params and string params
                                              (nflat (let loop ((ps params) (n 0))
                                                       (if (null? ps) n
                                                           (loop (cdr ps)
                                                                 (+ n (length (wit-type-core-types (cdar ps))))))))
                                              (nstring-params
                                               (let loop ((ps params) (n 0))
                                                 (if (null? ps) n
                                                     (loop (cdr ps)
                                                           (if (wit-type-needs-memory? (cdar ps))
                                                               (+ n 1) n)))))
                                              (result-is-string (and result (wit-type-needs-memory? result))))
                                         (if (> nstring-params 0)
                                             ;; Need extra locals: nstring eqref (string refs) + nstring i32 (loop counters)
                                             ;; + if result is string: 3 more i32 (ptr, len, loop-i) + 1 (ref null TY-STRING)
                                             (let ((n-extra-i32 (+ nstring-params (if result-is-string 3 0))))
                                               (if result-is-string
                                                   (begin (wbuf-u32! sbody 3)
                                                          (wbuf-u32! sbody nstring-params) (wbuf-byte! sbody HT-EQ)
                                                          (wbuf-u32! sbody 1) (wbuf-byte! sbody REF-NULL-TYPE) (wbuf-u32! sbody TY-STRING)
                                                          (wbuf-u32! sbody n-extra-i32) (wbuf-byte! sbody TYPE-I32))
                                                   (begin (wbuf-u32! sbody 2)
                                                          (wbuf-u32! sbody nstring-params) (wbuf-byte! sbody HT-EQ)
                                                          (wbuf-u32! sbody n-extra-i32) (wbuf-byte! sbody TYPE-I32))))
                                             (if result-is-string
                                                 ;; Only result needs string handling: 1 (ref null TY-STRING) + 3 i32 (ptr, len, loop-i)
                                                 (begin (wbuf-u32! sbody 2)
                                                        (wbuf-u32! sbody 1) (wbuf-byte! sbody REF-NULL-TYPE) (wbuf-u32! sbody TY-STRING)
                                                        (wbuf-u32! sbody 3) (wbuf-byte! sbody TYPE-I32))
                                                 (wbuf-u32! sbody 0)))
                                         ;; Convert each flat param to eqref for user function call
                                         (let loop ((flat-i 0) (ps params) (str-idx 0))
                                           (when (pair? ps)
                                             (if (wit-type-needs-memory? (cdar ps))
                                                 ;; String param: (ptr, len) -> GC string array
                                                 (let ((local-ref (+ nflat str-idx))
                                                       (local-i   (+ nflat nstring-params (if result-is-string 1 0) str-idx))
                                                       (ptr-flat flat-i)
                                                       (len-flat (+ flat-i 1)))
                                                   ;; Create array: array.new_default TY-STRING len
                                                   (wbuf-byte! sbody OP-LOCAL-GET) (wbuf-u32! sbody len-flat)
                                                   (wbuf-byte! sbody OP-GC-PREFIX) (wbuf-byte! sbody GC-ARRAY-NEW-DEFAULT)
                                                   (wbuf-u32! sbody TY-STRING)
                                                   (wbuf-byte! sbody OP-LOCAL-SET) (wbuf-u32! sbody local-ref)
                                                   ;; Copy loop: i=0; while(i<len) { str[i] = mem[ptr+i]; i++ }
                                                   (wbuf-byte! sbody OP-I32-CONST) (wbuf-i32! sbody 0)
                                                   (wbuf-byte! sbody OP-LOCAL-SET) (wbuf-u32! sbody local-i)
                                                   (wbuf-byte! sbody OP-BLOCK) (wbuf-byte! sbody TYPE-VOID)
                                                   (wbuf-byte! sbody OP-LOOP) (wbuf-byte! sbody TYPE-VOID)
                                                   ;; if i >= len, break
                                                   (wbuf-byte! sbody OP-LOCAL-GET) (wbuf-u32! sbody local-i)
                                                   (wbuf-byte! sbody OP-LOCAL-GET) (wbuf-u32! sbody len-flat)
                                                   (wbuf-byte! sbody OP-I32-GE-U)
                                                   (wbuf-byte! sbody OP-BR-IF) (wbuf-u32! sbody 1)
                                                   ;; str[i] = mem[ptr+i]
                                                   (wbuf-byte! sbody OP-LOCAL-GET) (wbuf-u32! sbody local-ref)
                                                   (wbuf-byte! sbody OP-LOCAL-GET) (wbuf-u32! sbody local-i)
                                                   (wbuf-byte! sbody OP-LOCAL-GET) (wbuf-u32! sbody ptr-flat)
                                                   (wbuf-byte! sbody OP-LOCAL-GET) (wbuf-u32! sbody local-i)
                                                   (wbuf-byte! sbody OP-I32-ADD)
                                                   (wbuf-byte! sbody OP-I32-LOAD8-U) (wbuf-u32! sbody 0) (wbuf-u32! sbody 0)
                                                   (wbuf-byte! sbody OP-GC-PREFIX) (wbuf-byte! sbody GC-ARRAY-SET)
                                                   (wbuf-u32! sbody TY-STRING)
                                                   ;; i++
                                                   (wbuf-byte! sbody OP-LOCAL-GET) (wbuf-u32! sbody local-i)
                                                   (wbuf-byte! sbody OP-I32-CONST) (wbuf-i32! sbody 1)
                                                   (wbuf-byte! sbody OP-I32-ADD)
                                                   (wbuf-byte! sbody OP-LOCAL-SET) (wbuf-u32! sbody local-i)
                                                   (wbuf-byte! sbody OP-BR) (wbuf-u32! sbody 0)
                                                   (wbuf-byte! sbody OP-END) ;; end loop
                                                   (wbuf-byte! sbody OP-END) ;; end block
                                                   ;; Push string ref as eqref for user func
                                                   (wbuf-byte! sbody OP-LOCAL-GET) (wbuf-u32! sbody local-ref)
                                                   (loop (+ flat-i 2) (cdr ps) (+ str-idx 1)))
                                                 ;; Non-string param: box i31
                                                 (begin
                                                   (wbuf-byte! sbody OP-LOCAL-GET)
                                                   (wbuf-u32! sbody flat-i)
                                                   (emit-box-fixnum! sbody)
                                                   (loop (+ flat-i 1) (cdr ps) str-idx)))))
                                         ;; Call user function
                                         (wbuf-byte! sbody OP-CALL)
                                         (wbuf-u32! sbody user-abs-idx)
                                         ;; Unbox result
                                         (if result
                                             (if result-is-string
                                                 ;; String result: convert GC string to (ptr, len)
                                                 (let ((local-str (+ nflat nstring-params))
                                                       (local-ptr (+ nflat nstring-params nstring-params 1))
                                                       (local-len (+ nflat nstring-params nstring-params 2))
                                                       (local-i   (+ nflat nstring-params nstring-params 3)))
                                                   ;; Cast to string, save ref, get length
                                                   (wbuf-byte! sbody OP-GC-PREFIX) (wbuf-byte! sbody GC-REF-CAST)
                                                   (wbuf-u32! sbody TY-STRING)
                                                   (wbuf-byte! sbody OP-LOCAL-TEE) (wbuf-u32! sbody local-str)
                                                   (wbuf-byte! sbody OP-GC-PREFIX) (wbuf-byte! sbody GC-ARRAY-LEN)
                                                   (wbuf-byte! sbody OP-LOCAL-SET) (wbuf-u32! sbody local-len)
                                                   ;; Bump-allocate
                                                   (wbuf-byte! sbody OP-GLOBAL-GET) (wbuf-u32! sbody bump-ptr-idx)
                                                   (wbuf-byte! sbody OP-LOCAL-TEE) (wbuf-u32! sbody local-ptr)
                                                   (wbuf-byte! sbody OP-LOCAL-GET) (wbuf-u32! sbody local-len)
                                                   (wbuf-byte! sbody OP-I32-ADD)
                                                   (wbuf-byte! sbody OP-GLOBAL-SET) (wbuf-u32! sbody bump-ptr-idx)
                                                   ;; Copy loop: i=0; while(i<len) { mem[ptr+i] = str[i]; i++ }
                                                   (wbuf-byte! sbody OP-I32-CONST) (wbuf-i32! sbody 0)
                                                   (wbuf-byte! sbody OP-LOCAL-SET) (wbuf-u32! sbody local-i)
                                                   (wbuf-byte! sbody OP-BLOCK) (wbuf-byte! sbody TYPE-VOID)
                                                   (wbuf-byte! sbody OP-LOOP) (wbuf-byte! sbody TYPE-VOID)
                                                   (wbuf-byte! sbody OP-LOCAL-GET) (wbuf-u32! sbody local-i)
                                                   (wbuf-byte! sbody OP-LOCAL-GET) (wbuf-u32! sbody local-len)
                                                   (wbuf-byte! sbody OP-I32-GE-U)
                                                   (wbuf-byte! sbody OP-BR-IF) (wbuf-u32! sbody 1)
                                                   ;; mem[ptr+i] = str[i]
                                                   (wbuf-byte! sbody OP-LOCAL-GET) (wbuf-u32! sbody local-ptr)
                                                   (wbuf-byte! sbody OP-LOCAL-GET) (wbuf-u32! sbody local-i)
                                                   (wbuf-byte! sbody OP-I32-ADD)
                                                   (wbuf-byte! sbody OP-LOCAL-GET) (wbuf-u32! sbody local-str)
                                                   (wbuf-byte! sbody OP-LOCAL-GET) (wbuf-u32! sbody local-i)
                                                   (wbuf-byte! sbody OP-GC-PREFIX) (wbuf-byte! sbody GC-ARRAY-GET-U)
                                                   (wbuf-u32! sbody TY-STRING)
                                                   (wbuf-byte! sbody OP-I32-STORE8) (wbuf-u32! sbody 0) (wbuf-u32! sbody 0)
                                                   ;; i++
                                                   (wbuf-byte! sbody OP-LOCAL-GET) (wbuf-u32! sbody local-i)
                                                   (wbuf-byte! sbody OP-I32-CONST) (wbuf-i32! sbody 1)
                                                   (wbuf-byte! sbody OP-I32-ADD)
                                                   (wbuf-byte! sbody OP-LOCAL-SET) (wbuf-u32! sbody local-i)
                                                   (wbuf-byte! sbody OP-BR) (wbuf-u32! sbody 0)
                                                   (wbuf-byte! sbody OP-END)
                                                   (wbuf-byte! sbody OP-END)
                                                   ;; Return ptr, len
                                                   (wbuf-byte! sbody OP-LOCAL-GET) (wbuf-u32! sbody local-ptr)
                                                   (wbuf-byte! sbody OP-LOCAL-GET) (wbuf-u32! sbody local-len))
                                                 (emit-unbox-fixnum! sbody))
                                             (wbuf-byte! sbody OP-DROP))
                                         (wbuf-byte! sbody OP-END)
                                         (wbuf-func-body! sec sbody)))
                                     wit-shim-info)

                                    ;; "run" function — call _start, return 0 (WASI only)
                                    (when (not *wit-world*)
                                      (let ((rbody (wbuf-make)))
                                        (wbuf-u32! rbody 0)
                                        (wbuf-byte! rbody OP-CALL) (wbuf-u32! rbody fn-start)
                                        (wbuf-byte! rbody OP-I32-CONST) (wbuf-i32! rbody 0)
                                        (wbuf-byte! rbody OP-END)
                                        (wbuf-func-body! sec rbody)))

                                    (wbuf-section! out SEC-CODE sec)
                                    (wbuf->bytevector out)))))))))))))))))
