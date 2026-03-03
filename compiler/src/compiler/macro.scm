;;; macro.scm — syntax-rules pattern matching and template instantiation
;;; Replaces macro.c: self-contained, no dependencies beyond standard Scheme.

;;; Data structures (vectors):
;;; Macro:   #(name literals patterns templates nrules introduced)
;;; Binding: #(name depth values)

;;; --- Macro accessors ---

(define (macro-name m) (vector-ref m 0))
(define (macro-literals m) (vector-ref m 1))
(define (macro-patterns m) (vector-ref m 2))
(define (macro-templates m) (vector-ref m 3))
(define (macro-nrules m) (vector-ref m 4))
(define (macro-introduced m) (vector-ref m 5))

;;; --- Binding accessors ---

(define (binding-name b) (vector-ref b 0))
(define (binding-depth b) (vector-ref b 1))
(define (binding-values b) (vector-ref b 2))

(define (make-binding name depth values)
  (vector name depth values))

;;; --- Gensym counter ---

(define *macro-counter* 0)

;;; --- Known forms (not gensym'd in templates) ---

(define *known-forms*
  '(+ - * / = < > <= >=
    display newline not boolean? eq? eqv? equal?
    number? integer? exact? inexact?
    zero? positive? negative? odd? even?
    abs quotient remainder modulo
    max min
    gcd lcm
    floor ceiling truncate round
    exact->inexact inexact->exact
    sqrt exp log sin cos tan
    asin acos atan expt
    number->string string->number
    cons car cdr null? pair?
    if define lambda begin set! quote
    let let* letrec cond case do and or else
    quasiquote unquote unquote-splicing
    define-syntax syntax-rules
    let-syntax letrec-syntax))

(define (known-form? sym)
  (let loop ((forms *known-forms*))
    (cond
      ((null? forms) #f)
      ((eq? sym (car forms)) #t)
      (else (loop (cdr forms))))))

;;; --- Literal check ---

(define (is-literal? name literals)
  (let loop ((lits literals))
    (cond
      ((null? lits) #f)
      ((eq? name (car lits)) #t)
      (else (loop (cdr lits))))))

;;; --- Ellipsis check ---

(define (is-ellipsis? v)
  (and (symbol? v) (string=? (symbol->string v) "...")))

;;; --- Collect pattern variables (for zero-iteration ellipsis) ---

(define (collect-pattern-vars pattern literals)
  (cond
    ((symbol? pattern)
     (let ((pstr (symbol->string pattern)))
       (if (or (string=? pstr "_")
               (string=? pstr "...")
               (is-literal? pattern literals))
           '()
           (list pattern))))
    ((pair? pattern)
     (let loop ((items pattern) (acc '()))
       (if (null? items)
           acc
           (let ((item (car items)))
             (if (is-ellipsis? item)
                 (loop (cdr items) acc)
                 (loop (cdr items)
                       (append acc (collect-pattern-vars item literals))))))))
    (else '())))

;;; --- Collect introduced symbols (binding positions in templates) ---

(define (collect-introduced-symbols tmpl)
  ;; Returns a list of symbols that appear in binding positions
  ;; (lambda params, let/let*/letrec bound names, define names/params)
  (cond
    ((not (pair? tmpl)) '())
    ((not (symbol? (car tmpl)))
     (collect-introduced-from-body tmpl))
    (else
     (let ((head-str (symbol->string (car tmpl))))
       (cond
         ((string=? head-str "lambda")
          ;; (lambda (x y ...) body ...)
          (if (and (pair? (cdr tmpl)) (pair? (cadr tmpl)))
              (append (collect-lambda-params (cadr tmpl))
                      (collect-introduced-from-body (cddr tmpl)))
              (collect-introduced-from-body (cddr tmpl))))
         ((or (string=? head-str "let") (string=? head-str "let*") (string=? head-str "letrec"))
          ;; (let ((x init) ...) body ...)
          (if (and (pair? (cdr tmpl)) (pair? (cadr tmpl)))
              (append (collect-let-names (cadr tmpl))
                      (collect-introduced-from-body (cddr tmpl)))
              (collect-introduced-from-body (cddr tmpl))))
         ((string=? head-str "define")
          ;; (define x expr) or (define (f x y) body ...)
          (if (pair? (cdr tmpl))
              (cond
                ((symbol? (cadr tmpl))
                 (cons (cadr tmpl) (collect-introduced-from-body (cddr tmpl))))
                ((pair? (cadr tmpl))
                 (append (collect-lambda-params (cadr tmpl))
                         (collect-introduced-from-body (cddr tmpl))))
                (else (collect-introduced-from-body (cddr tmpl))))
              '()))
         (else
          (collect-introduced-from-body tmpl)))))))

(define (collect-lambda-params formals)
  ;; Collect symbols from a lambda formal parameter list
  (cond
    ((null? formals) '())
    ((symbol? formals) (list formals))  ;; rest arg: (lambda args ...)
    ((pair? formals)
     (if (symbol? (car formals))
         (cons (car formals) (collect-lambda-params (cdr formals)))
         (collect-lambda-params (cdr formals))))
    (else '())))

(define (collect-let-names bindings)
  ;; Collect bound names from let-style binding list: ((x init) ...)
  (cond
    ((null? bindings) '())
    ((and (pair? bindings) (pair? (car bindings)) (symbol? (caar bindings)))
     (cons (caar bindings) (collect-let-names (cdr bindings))))
    ((pair? bindings)
     (collect-let-names (cdr bindings)))
    (else '())))

(define (collect-introduced-from-body exprs)
  ;; Recurse into a list of expressions
  (cond
    ((null? exprs) '())
    ((pair? exprs)
     (append (collect-introduced-symbols (car exprs))
             (collect-introduced-from-body (cdr exprs))))
    (else '())))

;;; --- Pattern matching ---

(define (match-pattern pattern input literals)
  ;; Returns list of bindings or #f
  (cond
    ;; Pattern variable (non-literal symbol, not _, not ...)
    ((symbol? pattern)
     (cond
       ((string=? (symbol->string pattern) "_") '())
       ((is-literal? pattern literals)
        (if (and (symbol? input) (eq? input pattern))
            '()
            #f))
       (else
        ;; Pattern variable: bind to input (depth 0)
        (list (make-binding pattern 0 (list input))))))

    ;; Integer constant
    ((and (integer? pattern) (exact? pattern))
     (if (and (integer? input) (exact? input) (= input pattern))
         '()
         #f))

    ;; Boolean constant
    ((boolean? pattern)
     (if (and (boolean? input) (eq? input pattern))
         '()
         #f))

    ;; Character constant
    ((char? pattern)
     (if (and (char? input) (char=? input pattern))
         '()
         #f))

    ;; String constant
    ((string? pattern)
     (if (and (string? input) (string=? input pattern))
         '()
         #f))

    ;; List pattern
    ((pair? pattern)
     (if (not (pair? input))
         #f
         (match-list pattern input literals)))

    ;; Null pattern
    ((null? pattern)
     (if (null? input) '() #f))

    (else #f)))

(define (match-list pats inputs literals)
  ;; pats and inputs are proper lists
  (let ((pat-list pats)
        (inp-list inputs))
    ;; Check for ellipsis: last element is `...`
    (let* ((pat-vec (list->vector pat-list))
           (npats (vector-length pat-vec))
           (has-ellipsis (and (>= npats 2)
                              (is-ellipsis? (vector-ref pat-vec (- npats 1))))))
      (if has-ellipsis
          (let* ((n-fixed (- npats 2))
                 (sub-pattern (vector-ref pat-vec (- npats 2)))
                 (inp-vec (list->vector inp-list))
                 (ninputs (vector-length inp-vec)))
            (if (< ninputs n-fixed)
                #f
                ;; Match fixed prefix
                (let loop-fixed ((i 0) (bindings '()))
                  (if (= i n-fixed)
                      ;; Now match repeated part
                      (let ((n-repeated (- ninputs n-fixed)))
                        (if (= n-repeated 0)
                            ;; Zero repetitions: register empty depth-1 bindings
                            (let ((vars (collect-pattern-vars sub-pattern literals)))
                              (let loop-vars ((vs vars) (acc bindings))
                                (if (null? vs)
                                    acc
                                    (loop-vars (cdr vs)
                                               (append acc
                                                       (list (make-binding (car vs) 1 '())))))))
                            ;; Match each repeated input
                            (let loop-rep ((j 0) (rep-bindings #f))
                              (if (= j n-repeated)
                                  (if rep-bindings
                                      (append bindings rep-bindings)
                                      bindings)
                                  (let ((tmp (match-pattern sub-pattern
                                                            (vector-ref inp-vec (+ n-fixed j))
                                                            literals)))
                                    (if (not tmp)
                                        #f
                                        (if (= j 0)
                                            ;; First iteration: create depth-1 bindings
                                            (let ((d1 (map (lambda (b)
                                                             (make-binding (binding-name b) 1
                                                                           (binding-values b)))
                                                           tmp)))
                                              (loop-rep (+ j 1) d1))
                                            ;; Subsequent: append to existing
                                            (begin
                                              (for-each
                                               (lambda (tb)
                                                 (let ((found (find-binding (binding-name tb)
                                                                            rep-bindings)))
                                                   (if found
                                                       (vector-set! found 2
                                                                    (append (binding-values found)
                                                                            (binding-values tb))))))
                                               tmp)
                                              (loop-rep (+ j 1) rep-bindings)))))))))
                      ;; Still matching fixed prefix
                      (let ((result (match-pattern (vector-ref pat-vec i)
                                                   (vector-ref inp-vec i)
                                                   literals)))
                        (if (not result)
                            #f
                            (loop-fixed (+ i 1) (append bindings result))))))))
          ;; Fixed mode: exact count match
          (let ((inp-vec (list->vector inp-list))
                (ninputs (length inp-list)))
            (if (not (= npats ninputs))
                #f
                (let loop ((i 0) (bindings '()))
                  (if (= i npats)
                      bindings
                      (let ((result (match-pattern (vector-ref pat-vec i)
                                                   (vector-ref inp-vec i)
                                                   literals)))
                        (if (not result)
                            #f
                            (loop (+ i 1) (append bindings result))))))))))))

;;; --- Helper: find binding by name ---

(define (find-binding name bindings)
  (cond
    ((null? bindings) #f)
    ((eq? name (binding-name (car bindings))) (car bindings))
    (else (find-binding name (cdr bindings)))))

;;; --- Binding lookup ---

(define (lookup-binding name bindings)
  ;; Returns the value or #f
  (let ((b (find-binding name bindings)))
    (if (and b (not (null? (binding-values b))))
        (car (binding-values b))
        #f)))

;;; --- Gensym map ---

(define (find-or-create-gensym orig gensym-map macro-id)
  ;; gensym-map is a mutable list cell: (list alist)
  ;; Returns the gensym name
  (let ((alist (car gensym-map)))
    (let ((entry (assq orig alist)))
      (if entry
          (cdr entry)
          (let ((gs (string->symbol
                     (string-append "__m" (number->string macro-id)
                                    "_" (symbol->string orig)))))
            (set-car! gensym-map (cons (cons orig gs) alist))
            gs)))))

;;; --- Repetition count ---

(define (repetition-count tmpl bindings)
  ;; Find depth-1 binding referenced by tmpl, return its value count
  (cond
    ((symbol? tmpl)
     (let ((b (find-binding tmpl bindings)))
       (if (and b (= (binding-depth b) 1))
           (length (binding-values b))
           -1)))
    ((pair? tmpl)
     (let loop ((items tmpl))
       (if (null? items)
           -1
           (let ((c (repetition-count (car items) bindings)))
             (if (>= c 0) c (loop (cdr items)))))))
    (else -1)))

;;; --- Make iteration env ---

(define (make-iteration-env bindings j)
  ;; depth-1 bindings become depth-0 with values[j]
  ;; If j >= length of values, keep as depth-1 (not referenced in this context)
  (map (lambda (b)
         (if (= (binding-depth b) 0)
             b
             (if (< j (length (binding-values b)))
                 (make-binding (binding-name b) 0
                               (list (list-ref (binding-values b) j)))
                 b)))
       bindings))

;;; --- Template instantiation ---

(define (instantiate tmpl bindings gensym-map macro-id introduced)
  (cond
    ((symbol? tmpl)
     (let ((b (find-binding tmpl bindings)))
       (if (and b (not (null? (binding-values b))))
           (car (binding-values b))
           (if (known-form? tmpl)
               tmpl
               ;; Hygiene: only gensym symbols introduced in binding positions
               (if (memq tmpl introduced)
                   (find-or-create-gensym tmpl gensym-map macro-id)
                   tmpl)))))

    ((pair? tmpl)
     (let loop ((items tmpl) (acc '()))
       (cond
         ((null? items) (reverse acc))
         ;; Check if this element is followed by `...`
         ((and (pair? (cdr items)) (is-ellipsis? (cadr items)))
          (let* ((sub-tmpl (car items))
                 (rcount (repetition-count sub-tmpl bindings))
                 (rcount (if (< rcount 0) 0 rcount)))
            (let rep-loop ((j 0) (acc acc))
              (if (= j rcount)
                  (loop (cddr items) acc)  ;; skip the `...`
                  (let ((iter-env (make-iteration-env bindings j)))
                    (rep-loop (+ j 1)
                              (cons (instantiate sub-tmpl iter-env
                                                 gensym-map macro-id introduced)
                                    acc)))))))
         ;; Standalone `...` — skip
         ((is-ellipsis? (car items))
          (loop (cdr items) acc))
         (else
          (loop (cdr items)
                (cons (instantiate (car items) bindings gensym-map macro-id introduced)
                      acc))))))

    (else tmpl)))

;;; --- Define-syntax detection ---

(define (is-define-syntax? form)
  (and (pair? form)
       (= (length form) 3)
       (symbol? (car form))
       (string=? (symbol->string (car form)) "define-syntax")))

;;; --- Parse syntax-rules ---

(define (parse-syntax-rules-binding name sr)
  ;; sr is the (syntax-rules (...) ...) form
  ;; Returns macro vector or #f
  (if (or (not (pair? sr))
          (< (length sr) 2)
          (not (symbol? (car sr)))
          (not (string=? (symbol->string (car sr)) "syntax-rules"))
          (not (list? (cadr sr))))
      #f
      (let* ((literals (cadr sr))
             (rules (cddr sr))
             (nrules (length rules))
             (patterns (make-vector nrules))
             (templates (make-vector nrules))
             (introduced (make-vector nrules)))
        (let loop ((rs rules) (i 0))
          (if (null? rs)
              (vector name literals patterns templates nrules introduced)
              (let ((rule (car rs)))
                (if (or (not (pair? rule)) (not (= (length rule) 2)))
                    #f
                    (let* ((pat (car rule))
                           (tmpl (cadr rule))
                           (pat-vars (collect-pattern-vars pat literals))
                           (tmpl-introduced (collect-introduced-symbols tmpl))
                           (filtered (let remove ((syms tmpl-introduced))
                                       (cond
                                         ((null? syms) '())
                                         ((memq (car syms) pat-vars)
                                          (remove (cdr syms)))
                                         (else (cons (car syms)
                                                     (remove (cdr syms))))))))
                      (vector-set! patterns i pat)
                      (vector-set! templates i tmpl)
                      (vector-set! introduced i filtered)
                      (loop (cdr rs) (+ i 1))))))))))

(define (parse-syntax-rules form)
  ;; form is (define-syntax name (syntax-rules ...))
  (if (not (is-define-syntax? form))
      #f
      (let ((name (cadr form)))
        (if (not (symbol? name))
            #f
            (parse-syntax-rules-binding name (caddr form))))))

;;; --- Try expand macro ---

(define *macro-pattern-error* (vector))  ;; sentinel for pattern-error

(define (try-expand-macro macro input)
  ;; Returns #f for no-match, *macro-pattern-error* for pattern-error,
  ;; or the expanded form directly for success
  (if (or (not (pair? input))
          (< (length input) 1)
          (not (symbol? (car input)))
          (not (eq? (car input) (macro-name macro))))
      #f
      (let ((patterns (macro-patterns macro))
            (templates (macro-templates macro))
            (nrules (macro-nrules macro))
            (literals (macro-literals macro)))
        (let loop ((r 0))
          (if (= r nrules)
              *macro-pattern-error*
              (let ((pattern (vector-ref patterns r)))
                (if (not (pair? pattern))
                    (loop (+ r 1))
                    ;; Skip keyword (first element) in both pattern and input
                    (let ((matched (match-list (cdr pattern) (cdr input) literals)))
                      (if (not matched)
                          (loop (+ r 1))
                          ;; Add macro name as a binding so it won't get gensym'd
                          (let* ((bindings (append matched
                                                   (list (make-binding (macro-name macro) 0
                                                                       (list (macro-name macro))))))
                                 (macro-id *macro-counter*)
                                 (gensym-map (list '()))
                                 (introduced (vector-ref (macro-introduced macro) r)))
                            (set! *macro-counter* (+ *macro-counter* 1))
                            (instantiate (vector-ref templates r)
                                         bindings gensym-map macro-id
                                         introduced)))))))))))
