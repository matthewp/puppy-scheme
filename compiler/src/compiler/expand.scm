;;; expand.scm — Two-pass expansion pipeline and desugaring
;;; Replaces expand.c: macro expansion + all desugaring transforms.

;;; --- Global state (reset at start of expand-forms) ---

(define *hoisted* '())
(define *needs-qq-append* #f)
(define *needs-gcd* #f)
(define *needs-rationalize* #f)

(define *case-counter* 0)
(define *or-counter* 0)
(define *letrec-counter* 0)
(define *do-counter* 0)
(define *abs-counter* 0)
(define *mod-counter* 0)
(define *maxmin-counter* 0)
(define *lcm-counter* 0)
(define *cwif-counter* 0)
(define *foreach-counter* 0)
(define *append-counter* 0)
(define *length-counter* 0)
(define *map-counter* 0)
(define *assq-counter* 0)
(define *assv-counter* 0)
(define *assoc-counter* 0)
(define *listref-counter* 0)
(define *listtail-counter* 0)
(define *reverse-counter* 0)
(define *listp-counter* 0)
(define *memq-counter* 0)
(define *memv-counter* 0)
(define *member-counter* 0)
(define *filter-counter* 0)
(define *take-counter* 0)
(define *receive-counter* 0)
(define *string-ctor-counter* 0)
(define *promise-counter* 0)

;;; --- Hoisting ---

(define (add-hoisted! def)
  (set! *hoisted* (cons def *hoisted*)))

;;; --- Variable renaming (scope-aware) ---

(define (filter-renames renames shadow-list)
  ;; renames is an alist ((from . to) ...)
  ;; Remove entries whose key appears in shadow-list
  (let loop ((rs renames) (acc '()))
    (if (null? rs)
        (reverse acc)
        (let ((entry (car rs)))
          (if (memq (car entry) shadow-list)
              (loop (cdr rs) acc)
              (loop (cdr rs) (cons entry acc)))))))

(define (rename-free form renames)
  (if (null? renames)
      form
      (cond
        ((symbol? form)
         (let ((entry (assq form renames)))
           (if entry (cdr entry) form)))

        ((not (pair? form)) form)

        ((null? form) form)

        ;; Must have at least one element
        ((not (symbol? (car form)))
         (map (lambda (x) (rename-free x renames)) form))

        (else
         (let ((head-str (symbol->string (car form))))
        ;; (quote ...) — never rename inside quotes
        (cond
        ((string=? head-str "quote") form)

        ;; (lambda (params...) body...)
        ((and (string=? head-str "lambda")
              (>= (length form) 3)
              (pair? (cadr form)))
         (let* ((params (cadr form))
                (filtered (filter-renames renames params)))
           (cons 'lambda
                 (cons params
                       (map (lambda (x) (rename-free x filtered))
                            (cddr form))))))

        ;; (let ((var init)...) body...)
        ((and (string=? head-str "let")
              (>= (length form) 3)
              (pair? (cadr form))
              (or (null? (cadr form))
                  (pair? (caadr form))))
         (let* ((bindings (cadr form))
                ;; Inits in outer scope
                (new-bindings
                 (map (lambda (b)
                        (if (and (pair? b) (= (length b) 2))
                            (list (car b) (rename-free (cadr b) renames))
                            b))
                      bindings))
                ;; Binding names shadow
                (bind-names (map (lambda (b)
                                   (if (and (pair? b) (>= (length b) 1) (symbol? (car b)))
                                       (car b)
                                       '()))
                                 bindings))
                (bind-names (filter symbol? bind-names))
                (filtered (filter-renames renames bind-names)))
           (cons 'let
                 (cons new-bindings
                       (map (lambda (x) (rename-free x filtered))
                            (cddr form))))))

        ;; (let* ((var init)...) body...) — progressive shadowing
        ((and (string=? head-str "let*")
              (>= (length form) 3)
              (pair? (cadr form)))
         (let* ((bindings (cadr form))
                ;; Process bindings with progressive shadowing
                (result (let loop ((bs bindings) (shadow '()) (acc '()))
                          (if (null? bs)
                              (cons (reverse acc) shadow)
                              (let* ((b (car bs))
                                     (filtered (filter-renames renames shadow))
                                     (new-b (if (and (pair? b) (= (length b) 2))
                                                (list (car b) (rename-free (cadr b) filtered))
                                                b))
                                     (new-shadow
                                      (if (and (pair? b) (>= (length b) 1) (symbol? (car b)))
                                          (cons (car b) shadow)
                                          shadow)))
                                (loop (cdr bs) new-shadow (cons new-b acc))))))
                (new-bindings (car result))
                (all-shadow (cdr result))
                (filtered (filter-renames renames all-shadow)))
           (cons 'let*
                 (cons new-bindings
                       (map (lambda (x) (rename-free x filtered))
                            (cddr form))))))

        ;; (define (name params...) body...)
        ((and (string=? head-str "define")
              (>= (length form) 3)
              (pair? (cadr form))
              (>= (length (cadr form)) 1))
         (let* ((sig (cadr form))
                (name-sym (car sig))
                (params (cdr sig))
                (new-name (rename-free name-sym renames))
                (filtered (filter-renames renames params)))
           (cons 'define
                 (cons (cons new-name params)
                       (map (lambda (x) (rename-free x filtered))
                            (cddr form))))))

        ;; Default: recurse into all elements
        (else
         (map (lambda (x) (rename-free x renames)) form))))))))

;;; --- Quasiquote expansion ---

(define (is-tagged-str? val tag-str)
  (and (pair? val) (= (length val) 2)
       (symbol? (car val))
       (string=? (symbol->string (car val)) tag-str)))

(define (expand-qq val)
  ;; (unquote expr) → expr
  (cond
    ((is-tagged-str? val "unquote")
     (cadr val))

    ;; (unquote-splicing expr) at top level — treat like unquote
    ((is-tagged-str? val "unquote-splicing")
     (cadr val))

    ;; non-list → (quote val)
    ((not (pair? val))
     (list 'quote val))

    ;; empty list → (quote ())
    ((null? val)
     (list 'quote '()))

    ;; nested quasiquote → treat as literal data
    ((is-tagged-str? val "quasiquote")
     (list 'quote val))

    ;; list → build with cons/append from right to left
    (else
     (let ((vec (list->vector val)))
       (expand-qq-loop vec (- (vector-length vec) 1) (list 'quote '()))))))

;;; Top-level recursive helper for expand-qq to avoid letrec re-entrancy
;;; when expand-qq calls itself recursively on nested quasiquoted forms.
(define (expand-qq-loop vec i result)
  (if (< i 0)
      result
      (let ((elem (vector-ref vec i)))
        (if (is-tagged-str? elem "unquote-splicing")
            (begin
              (set! *needs-qq-append* #t)
              (expand-qq-loop vec (- i 1)
                              (list '__qq_append (cadr elem) result)))
            (expand-qq-loop vec (- i 1)
                            (list 'cons (expand-qq elem) result))))))

(define (hoist-append-helper!)
  ;; (define (__qq_append l1 l2)
  ;;   (if (null? l1) l2
  ;;       (cons (car l1) (__qq_append (cdr l1) l2))))
  (add-hoisted!
   (list 'define (list '__qq_append 'l1 'l2)
         (list 'if (list 'null? 'l1) 'l2
               (list 'cons (list 'car 'l1)
                     (list '__qq_append (list 'cdr 'l1) 'l2))))))

;;; --- case desugaring ---

(define (desugar-case form)
  (let* ((key-expr (cadr form))
         (clauses (cddr form))
         (keyname (string->symbol
                   (string-append "__k" (number->string *case-counter*)))))
    (set! *case-counter* (+ *case-counter* 1))

    (let ((cond-clauses
           (map (lambda (clause)
                  (cond
                    ((not (pair? clause)) clause)
                    ((< (length clause) 2) clause)
                    ;; (else body...)
                    ((and (symbol? (car clause)) (string=? (symbol->string (car clause)) "else"))
                     clause)
                    ((not (pair? (car clause))) clause)
                    (else
                     (let* ((datums (car clause))
                            (body (cdr clause))
                            (test
                             (if (= (length datums) 1)
                                 (list 'eq? keyname (car datums))
                                 (cons 'or
                                       (map (lambda (d)
                                              (list 'eq? keyname d))
                                            datums)))))
                       (cons test body)))))
                clauses)))
      (list 'let (list (list keyname key-expr))
            (cons 'cond cond-clauses)))))

;;; --- letrec desugaring ---

(define (desugar-letrec form)
  (let* ((bindings (cadr form))
         (body (cddr form))
         (id *letrec-counter*)
         (renames
          (map (lambda (b)
                 (let* ((old-name (car b))
                        (new-name (string->symbol
                                   (string-append "__lr" (number->string id)
                                                  "_" (symbol->string old-name)))))
                   (cons old-name new-name)))
               bindings)))
    (set! *letrec-counter* (+ *letrec-counter* 1))

    ;; Build (let ((new 0) ...) (set! new init') ... body')
    ;; Use let instead of hoisted define to keep bindings local,
    ;; avoiding global clobbering when the enclosing function is recursive.
    (let* ((let-bindings (map (lambda (r)
                                (list (cdr r) 0))
                              renames))
           (sets (map (lambda (b r)
                        (let ((init (rename-free (cadr b) renames)))
                          (list 'set! (cdr r) init)))
                      bindings renames))
           (new-body (map (lambda (x) (rename-free x renames)) body)))
      (cons 'let (cons let-bindings (append sets new-body))))))

;;; --- do desugaring ---

(define (desugar-do form)
  (let* ((bindings (cadr form))
         (test-clause (caddr form))
         (body (cdddr form))
         (loopname (string->symbol
                    (string-append "__do" (number->string *do-counter*)))))
    (set! *do-counter* (+ *do-counter* 1))

    ;; Build the recursive call: (__doN step1 var2 step3 ...)
    (let* ((call-args
            (map (lambda (b)
                   (if (and (pair? b) (>= (length b) 3))
                       (caddr b)
                       (car b)))
                 bindings))
           (loop-call (cons loopname call-args))
           ;; Then branch: result expressions (or void)
           (nresults (- (length test-clause) 1))
           (then-branch
            (cond
              ((= nresults 0) 0)
              ((= nresults 1) (cadr test-clause))
              (else (cons 'begin (cdr test-clause)))))
           ;; Else branch: body + recursive call
           (else-branch
            (if (null? body)
                loop-call
                (cons 'begin (append body (list loop-call)))))
           ;; (if test then else)
           (if-expr (list 'if (car test-clause) then-branch else-branch))
           ;; (lambda (var1 var2 ...) if_expr)
           (params (map car bindings))
           (lambda-expr (list 'lambda params if-expr))
           ;; initial call: (__doN init1 init2 ...)
           (inits (map cadr bindings))
           (init-call (cons loopname inits)))
      ;; (letrec ((__doN lambda)) (__doN init1 init2 ...))
      (list 'letrec (list (list loopname lambda-expr)) init-call))))

;;; --- named let desugaring ---

(define (desugar-named-let form)
  (let* ((name (cadr form))
         (bindings (caddr form))
         (body (cdddr form))
         ;; (lambda (var ...) body ...)
         (params (map car bindings))
         (lambda-expr (cons 'lambda (cons params body)))
         ;; initial call: (name init ...)
         (inits (map cadr bindings))
         (init-call (cons name inits)))
    ;; (letrec ((name lambda)) (name init ...))
    (list 'letrec (list (list name lambda-expr)) init-call)))

;;; --- abs desugaring ---

(define (desugar-abs form)
  (let ((tmp (string->symbol
              (string-append "__abs" (number->string *abs-counter*)))))
    (set! *abs-counter* (+ *abs-counter* 1))
    (list 'let (list (list tmp (cadr form)))
          (list 'if (list 'negative? tmp)
                (list '- 0 tmp)
                tmp))))

;;; --- modulo desugaring ---

(define (desugar-modulo form)
  (let* ((id *mod-counter*)
         (na (string->symbol (string-append "__mod" (number->string id) "_a")))
         (nb (string->symbol (string-append "__mod" (number->string id) "_b")))
         (nr (string->symbol (string-append "__mod" (number->string id) "_r"))))
    (set! *mod-counter* (+ *mod-counter* 1))
    (list 'let (list (list na (cadr form))
                     (list nb (caddr form)))
          (list 'let (list (list nr (list 'remainder na nb)))
                (list 'if (list 'or (list 'zero? nr)
                                (list 'eq? (list 'negative? nr)
                                      (list 'negative? nb)))
                      nr
                      (list '+ nr nb))))))

;;; --- max/min desugaring ---

(define (desugar-maxmin form cmp)
  (let* ((op (car form))
         (id *maxmin-counter*)
         (na (string->symbol
              (string-append "__" (symbol->string op) (number->string id) "_a")))
         (nb (string->symbol
              (string-append "__" (symbol->string op) (number->string id) "_b"))))
    (set! *maxmin-counter* (+ *maxmin-counter* 1))
    (list 'let (list (list na (cadr form))
                     (list nb (caddr form)))
          (list 'if (list cmp na nb)
                na nb))))

;;; --- gcd/lcm ---

(define (hoist-gcd-helper!)
  ;; (define (__gcd a b) (if (zero? b) (if (negative? a) (- 0 a) a) (__gcd b (remainder a b))))
  (add-hoisted!
   (list 'define (list '__gcd 'a 'b)
         (list 'if (list 'zero? 'b)
               (list 'if (list 'negative? 'a)
                     (list '- 0 'a)
                     'a)
               (list '__gcd 'b (list 'remainder 'a 'b))))))

(define (desugar-gcd form)
  (set! *needs-gcd* #t)
  (list '__gcd (cadr form) (caddr form)))

(define (desugar-lcm form)
  (set! *needs-gcd* #t)
  (let* ((id *lcm-counter*)
         (na (string->symbol (string-append "__lcm" (number->string id) "_a")))
         (nb (string->symbol (string-append "__lcm" (number->string id) "_b"))))
    (set! *lcm-counter* (+ *lcm-counter* 1))
    (list 'let (list (list na (cadr form))
                     (list nb (caddr form)))
          (list 'if (list 'or (list 'zero? na) (list 'zero? nb))
                0
                (list 'abs (list '* (list 'quotient na
                                          (list '__gcd na nb))
                                 nb))))))

;;; --- rationalize desugaring ---

(define (hoist-rationalize-helper!)
  ;; (define (__rationalize-simplest lo hi)
  ;;   (cond ((= lo hi) lo)
  ;;         ((and (<= lo 0) (>= hi 0)) 0)
  ;;         ((< hi 0) (- (__rationalize-simplest (- hi) (- lo))))
  ;;         (else (let ((fl (floor lo)) (fh (floor hi)))
  ;;                 (if (< fl fh) (+ fl 1)
  ;;                   (+ fl (/ 1 (__rationalize-simplest (/ 1 (- hi fl)) (/ 1 (- lo fl))))))))))
  ;; (define (__rationalize x delta) (__rationalize-simplest (- x delta) (+ x delta)))
  (add-hoisted!
   (list 'define (list '__rationalize-simplest 'lo 'hi)
         (list 'cond
               (list (list '= 'lo 'hi) 'lo)
               (list (list 'and (list '<= 'lo 0) (list '>= 'hi 0)) 0)
               (list (list '< 'hi 0)
                     (list '- (list '__rationalize-simplest (list '- 'hi) (list '- 'lo))))
               (list 'else
                     (list 'let (list (list 'fl (list 'floor 'lo))
                                      (list 'fh (list 'floor 'hi)))
                           (list 'if (list '< 'fl 'fh)
                                 (list '+ 'fl 1)
                                 (list '+ 'fl
                                       (list '/ 1
                                             (list '__rationalize-simplest
                                                   (list '/ 1 (list '- 'hi 'fl))
                                                   (list '/ 1 (list '- 'lo 'fl)))))))))))
  (add-hoisted!
   (list 'define (list '__rationalize 'x 'delta)
         (list '__rationalize-simplest (list '- 'x 'delta) (list '+ 'x 'delta)))))

(define (desugar-rationalize form)
  (set! *needs-rationalize* #t)
  (list '__rationalize (cadr form) (caddr form)))

;;; --- for-each desugaring ---

(define (desugar-for-each form)
  (let* ((proc (cadr form))
         (lst (caddr form))
         (loopname (string->symbol
                    (string-append "__foreach" (number->string *foreach-counter*)))))
    (set! *foreach-counter* (+ *foreach-counter* 1))
    (list 'let loopname (list (list 'l lst))
          (list 'if '(null? l) 0
                (list 'begin
                      (list proc '(car l))
                      (list loopname '(cdr l)))))))

;;; --- append desugaring ---

(define (desugar-append form)
  (let* ((l1 (cadr form))
         (l2 (caddr form))
         (loopname (string->symbol
                    (string-append "__append" (number->string *append-counter*)))))
    (set! *append-counter* (+ *append-counter* 1))
    (list 'let loopname (list (list 'l l1))
          (list 'if '(null? l) l2
                (list 'cons '(car l)
                      (list loopname '(cdr l)))))))

;;; --- length desugaring ---

(define (desugar-length form)
  (let* ((lst (cadr form))
         (loopname (string->symbol
                    (string-append "__length" (number->string *length-counter*)))))
    (set! *length-counter* (+ *length-counter* 1))
    (list 'let loopname (list (list 'l lst) (list 'n 0))
          (list 'if '(null? l) 'n
                (list loopname '(cdr l) '(+ n 1))))))

;;; --- map desugaring ---

(define (desugar-map form)
  (let* ((proc (cadr form))
         (nargs (- (length form) 2))
         (loopname (string->symbol
                    (string-append "__map" (number->string *map-counter*)))))
    (set! *map-counter* (+ *map-counter* 1))
    (if (= nargs 1)
        ;; Single-list map
        (let ((lst (caddr form)))
          (list 'let loopname (list (list 'l lst))
                (list 'if '(null? l) ''()
                      (list 'cons (list proc '(car l))
                            (list loopname '(cdr l))))))
        ;; Two-list map
        (let ((l1 (caddr form))
              (l2 (cadddr form)))
          (list 'let loopname (list (list 'a l1) (list 'b l2))
                (list 'if '(null? a) ''()
                      (list 'cons (list proc '(car a) '(car b))
                            (list loopname '(cdr a) '(cdr b)))))))))

;;; --- assq desugaring ---

(define (desugar-assq form)
  (let* ((key (cadr form))
         (lst (caddr form))
         (loopname (string->symbol
                    (string-append "__assq" (number->string *assq-counter*))))
         (keyname (string->symbol
                   (string-append "__assq" (number->string *assq-counter*) "_k"))))
    (set! *assq-counter* (+ *assq-counter* 1))
    (list 'let (list (list keyname key))
          (list 'let loopname (list (list 'l lst))
                (list 'if '(null? l) #f
                      (list 'if (list 'eq? keyname '(car (car l)))
                            '(car l)
                            (list loopname '(cdr l))))))))

;;; --- assv desugaring ---

(define (desugar-assv form)
  (let* ((key (cadr form))
         (lst (caddr form))
         (loopname (string->symbol
                    (string-append "__assv" (number->string *assv-counter*))))
         (keyname (string->symbol
                   (string-append "__assv" (number->string *assv-counter*) "_k"))))
    (set! *assv-counter* (+ *assv-counter* 1))
    (list 'let (list (list keyname key))
          (list 'let loopname (list (list 'l lst))
                (list 'if '(null? l) #f
                      (list 'if (list 'eqv? keyname '(car (car l)))
                            '(car l)
                            (list loopname '(cdr l))))))))

;;; --- assoc desugaring ---

(define (desugar-assoc form)
  (let* ((key (cadr form))
         (lst (caddr form))
         (loopname (string->symbol
                    (string-append "__assoc" (number->string *assoc-counter*))))
         (keyname (string->symbol
                   (string-append "__assoc" (number->string *assoc-counter*) "_k"))))
    (set! *assoc-counter* (+ *assoc-counter* 1))
    (list 'let (list (list keyname key))
          (list 'let loopname (list (list 'l lst))
                (list 'if '(null? l) #f
                      (list 'if (list 'equal? keyname '(car (car l)))
                            '(car l)
                            (list loopname '(cdr l))))))))

;;; --- filter desugaring ---

(define (desugar-filter form)
  (let* ((pred (cadr form))
         (lst (caddr form))
         (loopname (string->symbol
                    (string-append "__filter" (number->string *filter-counter*)))))
    (set! *filter-counter* (+ *filter-counter* 1))
    (list 'let loopname (list (list 'l lst))
          (list 'if '(null? l) ''()
                (list 'if (list pred '(car l))
                      (list 'cons '(car l) (list loopname '(cdr l)))
                      (list loopname '(cdr l)))))))

;;; --- take desugaring ---

(define (desugar-take form)
  (let* ((lst (cadr form))
         (n (caddr form))
         (loopname (string->symbol
                    (string-append "__take" (number->string *take-counter*)))))
    (set! *take-counter* (+ *take-counter* 1))
    (list 'let loopname (list (list 'l lst) (list 'n n))
          (list 'if '(zero? n) ''()
                (list 'cons '(car l) (list loopname '(cdr l) '(- n 1)))))))

;;; --- receive desugaring ---

(define (desugar-receive form)
  ;; (receive (x y z) expr body...) →
  ;; (let ((__recv0 expr))
  ;;   (let ((x (car __recv0))
  ;;         (y (car (cdr __recv0)))
  ;;         (z (car (cdr (cdr __recv0)))))
  ;;     body...))
  (let* ((formals (cadr form))
         (expr (caddr form))
         (body (cdddr form))
         (id *receive-counter*)
         (tmp (string->symbol
               (string-append "__recv" (number->string id)))))
    (set! *receive-counter* (+ *receive-counter* 1))
    (let ((bindings
           (let loop ((vars formals) (i 0) (acc '()))
             (if (null? vars)
                 (reverse acc)
                 (let ((accessor
                        (let iloop ((j 0) (e tmp))
                          (if (= j i)
                              (list 'car e)
                              (iloop (+ j 1) (list 'cdr e))))))
                   (loop (cdr vars) (+ i 1)
                         (cons (list (car vars) accessor) acc)))))))
      (list 'let (list (list tmp expr))
            (cons 'let (cons bindings body))))))

;;; --- memq desugaring ---

(define (desugar-memq form)
  (let* ((key (cadr form))
         (lst (caddr form))
         (loopname (string->symbol
                    (string-append "__memq" (number->string *memq-counter*))))
         (keyname (string->symbol
                   (string-append "__memq" (number->string *memq-counter*) "_k"))))
    (set! *memq-counter* (+ *memq-counter* 1))
    (list 'let (list (list keyname key))
          (list 'let loopname (list (list 'l lst))
                (list 'if '(null? l) #f
                      (list 'if (list 'eq? keyname '(car l))
                            'l
                            (list loopname '(cdr l))))))))

;;; --- memv desugaring ---

(define (desugar-memv form)
  (let* ((key (cadr form))
         (lst (caddr form))
         (loopname (string->symbol
                    (string-append "__memv" (number->string *memv-counter*))))
         (keyname (string->symbol
                   (string-append "__memv" (number->string *memv-counter*) "_k"))))
    (set! *memv-counter* (+ *memv-counter* 1))
    (list 'let (list (list keyname key))
          (list 'let loopname (list (list 'l lst))
                (list 'if '(null? l) #f
                      (list 'if (list 'eqv? keyname '(car l))
                            'l
                            (list loopname '(cdr l))))))))

;;; --- member desugaring ---

(define (desugar-member form)
  (let* ((key (cadr form))
         (lst (caddr form))
         (loopname (string->symbol
                    (string-append "__member" (number->string *member-counter*))))
         (keyname (string->symbol
                   (string-append "__member" (number->string *member-counter*) "_k"))))
    (set! *member-counter* (+ *member-counter* 1))
    (list 'let (list (list keyname key))
          (list 'let loopname (list (list 'l lst))
                (list 'if '(null? l) #f
                      (list 'if (list 'equal? keyname '(car l))
                            'l
                            (list loopname '(cdr l))))))))

;;; --- list? desugaring ---

(define (desugar-list? form)
  (let* ((lst (cadr form))
         (loopname (string->symbol
                    (string-append "__listp" (number->string *listp-counter*)))))
    (set! *listp-counter* (+ *listp-counter* 1))
    (list 'let loopname (list (list 'l lst))
          (list 'if '(null? l) #t
                (list 'if '(pair? l)
                      (list loopname '(cdr l))
                      #f)))))

;;; --- cXXr desugaring ---

(define (desugar-cxxr op form)
  ;; op is a symbol like cadr, caddr, etc.
  ;; Parse the a/d letters between c and r, apply from right to left.
  (let* ((s (symbol->string op))
         (letters (substring s 1 (- (string-length s) 1)))
         (arg (cadr form)))
    (let loop ((i (- (string-length letters) 1)) (expr arg))
      (if (< i 0)
          expr
          (let ((c (string-ref letters i)))
            (loop (- i 1)
                  (if (char=? c #\a)
                      (list 'car expr)
                      (list 'cdr expr))))))))

;;; --- reverse desugaring ---

(define (desugar-reverse form)
  (let* ((lst (cadr form))
         (loopname (string->symbol
                    (string-append "__reverse" (number->string *reverse-counter*)))))
    (set! *reverse-counter* (+ *reverse-counter* 1))
    (list 'let loopname (list (list 'l lst) (list 'acc ''()))
          (list 'if '(null? l) 'acc
                (list loopname '(cdr l) (list 'cons '(car l) 'acc))))))

;;; --- list-ref desugaring ---

(define (desugar-list-ref form)
  (let* ((lst (cadr form))
         (idx (caddr form))
         (loopname (string->symbol
                    (string-append "__listref" (number->string *listref-counter*)))))
    (set! *listref-counter* (+ *listref-counter* 1))
    (list 'let loopname (list (list 'l lst) (list 'i idx))
          (list 'if '(zero? i) '(car l)
                (list loopname '(cdr l) '(- i 1))))))

;;; --- list-tail desugaring ---

(define (desugar-list-tail form)
  (let* ((lst (cadr form))
         (idx (caddr form))
         (loopname (string->symbol
                    (string-append "__listtail" (number->string *listtail-counter*)))))
    (set! *listtail-counter* (+ *listtail-counter* 1))
    (list 'let loopname (list (list 'l lst) (list 'i idx))
          (list 'if '(zero? i) 'l
                (list loopname '(cdr l) '(- i 1))))))

;;; --- apply desugaring ---

(define (desugar-apply form)
  ;; (apply f a b rest) → (apply f (cons a (cons b rest)))
  (let ((args (cddr form)))
    (let loop ((as (reverse (cdr args))) (acc (car (reverse args))))
      (if (null? as)
          (list 'apply (cadr form) acc)
          (loop (cdr as) (list 'cons (car as) acc))))))

;;; --- call-with-input-file / call-with-output-file desugaring ---

(define (desugar-call-with-input-file form)
  (let* ((id *cwif-counter*)
         (port-name (string->symbol
                     (string-append "__cwif" (number->string id) "_port")))
         (result-name (string->symbol
                       (string-append "__cwif" (number->string id) "_result"))))
    (set! *cwif-counter* (+ *cwif-counter* 1))
    (list 'let (list (list port-name (list 'open-input-file (cadr form))))
          (list 'let (list (list result-name
                                 (list (caddr form) port-name)))
                (list 'close-input-port port-name)
                result-name))))

(define (desugar-call-with-output-file form)
  (let* ((id *cwif-counter*)
         (port-name (string->symbol
                     (string-append "__cwof" (number->string id) "_port")))
         (result-name (string->symbol
                       (string-append "__cwof" (number->string id) "_result"))))
    (set! *cwif-counter* (+ *cwif-counter* 1))
    (list 'let (list (list port-name (list 'open-output-file (cadr form))))
          (list 'let (list (list result-name
                                 (list (caddr form) port-name)))
                (list 'close-output-port port-name)
                result-name))))

;;; --- Internal defines → letrec ---

(define (desugar-internal-defines body)
  ;; Collect leading defines from body, wrap remainder in letrec
  (let loop ((forms body) (defs '()))
    (if (and (pair? forms)
             (pair? (car forms))
             (symbol? (caar forms))
             (string=? (symbol->string (caar forms)) "define"))
        (loop (cdr forms) (cons (car forms) defs))
        (if (null? defs)
            body
            (list
             (cons 'letrec
                   (cons (map (lambda (d)
                                (if (and (>= (length d) 3)
                                         (pair? (cadr d))
                                         (symbol? (caadr d)))
                                    ;; (define (name p...) body...) → (name (lambda (p...) body...))
                                    (list (caadr d)
                                          (cons 'lambda (cons (cdadr d) (cddr d))))
                                    ;; (define name expr) → (name expr)
                                    (list (cadr d) (caddr d))))
                              (reverse defs))
                         forms)))))))

(define (desugar-body-defines form op-str)
  ;; op-str already computed by caller — skip forms that aren't lambda/let/let*/define
  (if (not op-str) form
  (let ((fc (string-ref op-str 0)))
    (cond
      ;; (lambda (params) body...)
      ((and (char=? fc #\l)
            (string=? op-str "lambda")
            (>= (length form) 3))
       (let ((new-body (desugar-internal-defines (cddr form))))
         (if (eq? new-body (cddr form)) form
             (cons 'lambda (cons (cadr form) new-body)))))
      ;; (let bindings body...)
      ((and (char=? fc #\l)
            (string=? op-str "let")
            (>= (length form) 3) (pair? (cadr form)))
       (let ((new-body (desugar-internal-defines (cddr form))))
         (if (eq? new-body (cddr form)) form
             (cons 'let (cons (cadr form) new-body)))))
      ;; (let* bindings body...)
      ((and (char=? fc #\l)
            (string=? op-str "let*")
            (>= (length form) 3) (pair? (cadr form)))
       (let ((new-body (desugar-internal-defines (cddr form))))
         (if (eq? new-body (cddr form)) form
             (cons 'let* (cons (cadr form) new-body)))))
      ;; (define (name params...) body...)
      ((and (char=? fc #\d)
            (string=? op-str "define")
            (>= (length form) 3) (pair? (cadr form)))
       (let ((new-body (desugar-internal-defines (cddr form))))
         (if (eq? new-body (cddr form)) form
             (cons 'define (cons (cadr form) new-body)))))
      (else form)))))

;;; --- string constructor desugaring ---

(define (desugar-string-ctor form)
  ;; (string #\a #\b #\c) → (let ((__str (make-string 3))) (string-set! __str 0 #\a) ... __str)
  (let* ((chars (cdr form))
         (n (length chars))
         (id *string-ctor-counter*)
         (tmp (string->symbol (string-append "__str" (number->string id)))))
    (set! *string-ctor-counter* (+ *string-ctor-counter* 1))
    (let loop ((cs chars) (i 0) (sets '()))
      (if (null? cs)
          (list 'let (list (list tmp (list 'make-string n)))
                (cons 'begin (append (reverse sets) (list tmp))))
          (loop (cdr cs) (+ i 1)
                (cons (list 'string-set! tmp i (car cs)) sets))))))

;;; --- Post-desugar dispatch ---

;; Hash table of keywords that post-desugar handles.
;; Non-matching ops skip the entire dispatch in O(1).
;; Lazily initialized since make-string-ht is defined in analyze.scm (included after expand.scm).
(define *desugar-keywords* #f)
(define (init-desugar-keywords!)
  (when (not *desugar-keywords*)
    (let ((ht (make-string-ht)))
      (for-each (lambda (k) (string-ht-set! ht k #t))
        '("case" "letrec" "let" "do" "abs" "modulo" "max" "min" "gcd" "lcm"
          "when" "unless" "for-each" "append" "length" "map" "assq" "assv" "assoc"
          "filter" "take" "memq" "memv" "member" "list?" "reverse" "list-ref" "list-tail"
          "newline" "values" "receive" "call-with-input-file" "call-with-output-file"
          "string" "string>?" "string<=?" "string>=?"
          "string-ci>?" "string-ci<=?" "string-ci>=?"
          "rationalize"
          "+" "-" "*" "/" "apply" "delay" "force"))
      (set! *desugar-keywords* ht))))

(define (post-desugar form macros depth op-str)
  (if (not op-str)
      form
      (let ((op (car form)))
        ;; Fast path: skip dispatch for non-desugar operators
        (if (not (string-ht-has? *desugar-keywords* op-str))
            ;; Still check cxxr pattern (c..r accessor)
            (if (and (= (length form) 2)
                     (>= (string-length op-str) 4)
                     (<= (string-length op-str) 6)
                     (char=? (string-ref op-str 0) #\c)
                     (char=? (string-ref op-str (- (string-length op-str) 1)) #\r)
                     (let check-ad ((ci 1))
                       (if (>= ci (- (string-length op-str) 1)) #t
                           (let ((ch (string-ref op-str ci)))
                             (and (or (char=? ch #\a) (char=? ch #\d))
                                  (check-ad (+ ci 1)))))))
                (let ((result (desugar-cxxr op form)))
                  (expand-one result macros (+ depth 1)))
                form)
        (cond
          ((and (string=? op-str "case") (>= (length form) 3))
           (desugar-case form))

          ((and (string=? op-str "letrec")
                (>= (length form) 3)
                (pair? (cadr form)))
           (desugar-letrec form))

          ((and (string=? op-str "let")
                (>= (length form) 4)
                (symbol? (cadr form))
                (list? (caddr form)))
           (let ((result (desugar-named-let form)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "do")
                (>= (length form) 3)
                (pair? (cadr form))
                (pair? (caddr form)))
           (let ((result (desugar-do form)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "abs") (= (length form) 2))
           (let ((result (desugar-abs form)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "modulo") (= (length form) 3))
           (let ((result (desugar-modulo form)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "max") (= (length form) 3))
           (let ((result (desugar-maxmin form '>)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "min") (= (length form) 3))
           (let ((result (desugar-maxmin form '<)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "gcd") (= (length form) 3))
           (desugar-gcd form))

          ((and (string=? op-str "lcm") (= (length form) 3))
           (let ((result (desugar-lcm form)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "rationalize") (= (length form) 3))
           (desugar-rationalize form))

          ((and (string=? op-str "when") (>= (length form) 3))
           (let ((result (list 'if (cadr form) (cons 'begin (cddr form)))))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "unless") (>= (length form) 3))
           (let ((result (list 'if (list 'not (cadr form)) (cons 'begin (cddr form)))))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "for-each") (= (length form) 3))
           (let ((result (desugar-for-each form)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "apply") (> (length form) 3))
           (let ((result (desugar-apply form)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "append") (= (length form) 3))
           (let ((result (desugar-append form)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "length") (= (length form) 2))
           (let ((result (desugar-length form)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "map") (or (= (length form) 3) (= (length form) 4)))
           (let ((result (desugar-map form)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "assq") (= (length form) 3))
           (let ((result (desugar-assq form)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "assv") (= (length form) 3))
           (let ((result (desugar-assv form)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "assoc") (= (length form) 3))
           (let ((result (desugar-assoc form)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "filter") (= (length form) 3))
           (let ((result (desugar-filter form)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "take") (= (length form) 3))
           (let ((result (desugar-take form)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "memq") (= (length form) 3))
           (let ((result (desugar-memq form)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "memv") (= (length form) 3))
           (let ((result (desugar-memv form)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "member") (= (length form) 3))
           (let ((result (desugar-member form)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "list?") (= (length form) 2))
           (let ((result (desugar-list? form)))
             (expand-one result macros (+ depth 1))))

          ((and (= (length form) 2)
                (>= (string-length op-str) 4)
                (<= (string-length op-str) 6)
                (char=? (string-ref op-str 0) #\c)
                (char=? (string-ref op-str (- (string-length op-str) 1)) #\r)
                (let check-ad ((ci 1))
                  (if (>= ci (- (string-length op-str) 1)) #t
                      (let ((ch (string-ref op-str ci)))
                        (and (or (char=? ch #\a) (char=? ch #\d))
                             (check-ad (+ ci 1)))))))
           (let ((result (desugar-cxxr op form)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "reverse") (= (length form) 2))
           (let ((result (desugar-reverse form)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "list-ref") (= (length form) 3))
           (let ((result (desugar-list-ref form)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "list-tail") (= (length form) 3))
           (let ((result (desugar-list-tail form)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "newline") (= (length form) 2))
           (list 'display "\n" (cadr form)))

          ;; (values a b c) → (list a b c)
          ((string=? op-str "values")
           (expand-one (cons 'list (cdr form)) macros (+ depth 1)))

          ;; (receive (x y z) expr body...)
          ((and (string=? op-str "receive") (>= (length form) 4) (pair? (cadr form)))
           (let ((result (desugar-receive form)))
             (expand-one result macros (+ depth 1))))

          ;; call-with-values: (call-with-values producer consumer) → (receive __tmp (producer) (consumer . __tmp-elems))
          ;; TODO: implement when needed (requires apply or known-arity destructuring)

          ((and (string=? op-str "call-with-input-file") (= (length form) 3))
           (let ((result (desugar-call-with-input-file form)))
             (expand-one result macros (+ depth 1))))

          ((and (string=? op-str "call-with-output-file") (= (length form) 3))
           (let ((result (desugar-call-with-output-file form)))
             (expand-one result macros (+ depth 1))))

          ;; (string ch ...) → constructor
          ((and (string=? op-str "string") (>= (length form) 1))
           (let ((result (desugar-string-ctor form)))
             (expand-one result macros (+ depth 1))))

          ;; string comparison desugaring
          ((and (string=? op-str "string>?") (= (length form) 3))
           (expand-one (list 'string<? (caddr form) (cadr form)) macros (+ depth 1)))
          ((and (string=? op-str "string<=?") (= (length form) 3))
           (expand-one (list 'not (list 'string<? (caddr form) (cadr form))) macros (+ depth 1)))
          ((and (string=? op-str "string>=?") (= (length form) 3))
           (expand-one (list 'not (list 'string<? (cadr form) (caddr form))) macros (+ depth 1)))
          ((and (string=? op-str "string-ci>?") (= (length form) 3))
           (expand-one (list 'string-ci<? (caddr form) (cadr form)) macros (+ depth 1)))
          ((and (string=? op-str "string-ci<=?") (= (length form) 3))
           (expand-one (list 'not (list 'string-ci<? (caddr form) (cadr form))) macros (+ depth 1)))
          ((and (string=? op-str "string-ci>=?") (= (length form) 3))
           (expand-one (list 'not (list 'string-ci<? (cadr form) (caddr form))) macros (+ depth 1)))

          ;; n-ary arithmetic: left-fold (+ a b c) → (+ (+ a b) c)
          ((and (or (string=? op-str "+") (string=? op-str "-")
                    (string=? op-str "*") (string=? op-str "/"))
                (> (length form) 3))
           (let fold ((args (cdddr form))
                      (acc (list op (cadr form) (caddr form))))
             (if (null? args)
                 (expand-one acc macros (+ depth 1))
                 (fold (cdr args) (list op acc (car args))))))

          ;; unary minus: (- x) → (- 0 x)
          ((and (string=? op-str "-") (= (length form) 2))
           (expand-one (list '- 0 (cadr form)) macros (+ depth 1)))

          ;; delay: (delay expr) → (%make-promise (lambda () expr))
          ((and (string=? op-str "delay") (= (length form) 2))
           (list '%make-promise (list 'lambda '() (cadr form))))

          ;; force: (force expr) → inline check-and-memoize
          ((and (string=? op-str "force") (= (length form) 2))
           (let* ((n *promise-counter*)
                  (pname (string->symbol (string-append "__p" (number->string n))))
                  (tname (string->symbol (string-append "__t" (number->string n))))
                  (vname (string->symbol (string-append "__v" (number->string n)))))
             (set! *promise-counter* (+ n 1))
             (expand-one
               (list 'let (list (list pname (cadr form)))
                 (list 'if (list '%i31-eqz (list '%promise-state pname))
                   (list 'let (list (list tname (list '%promise-ref pname)))
                     (list 'let (list (list vname (list tname)))
                       (list '%promise-set! pname vname)
                       (list '%promise-set-state! pname 1)
                       vname))
                   (list '%promise-ref pname)))
               macros (+ depth 1))))

          (else form))))))

;;; --- Recursive expansion ---

(define (expand-one form macros depth)
  (if (or (not (pair? form)) (null? form))
      form
      ;; macro expansion: try before any desugaring
      (let ((form (try-macro-expand form macros depth)))
        (if (or (not (pair? form)) (null? form))
            form
            ;; Compute op-str once for all checks
            (let ((op-str (and (symbol? (car form)) (symbol->string (car form)))))
              (cond
                ;; let-syntax / letrec-syntax
                ((and op-str
                      (>= (length form) 3)
                      (char=? (string-ref op-str 0) #\l)
                      (or (string=? op-str "let-syntax")
                          (string=? op-str "letrec-syntax")))
                 (expand-let-syntax form macros depth))
                ;; quasiquote
                ((and op-str
                      (>= (length form) 2)
                      (string=? op-str "quasiquote"))
                 (let ((result (expand-qq (cadr form))))
                   (expand-one result macros depth)))
                ;; Normal form: desugar internal defines, recurse, then post-desugar
                ;; quote — don't recurse
                ((and op-str (string=? op-str "quote"))
                 form)
                ;; Normal form: desugar internal defines, recurse, then post-desugar
                (else
                 (let* ((pre (desugar-body-defines form op-str))
                        (expanded (expand-children pre macros depth))
                        (exp-op-str (if (eq? expanded pre)
                                        op-str
                                        (and (pair? expanded) (symbol? (car expanded))
                                             (symbol->string (car expanded)))))
                        (desugared (post-desugar expanded macros depth exp-op-str)))
                   desugared))))))))


(define (try-macro-expand form macros depth)
  (if (or (>= depth 1000) (not (symbol? (car form))))
      form
      ;; Try macros (already in reverse definition order from collect-macros)
      (let loop ((ms macros))
        (if (null? ms)
            form
            (let* ((m (car ms))
                   (result (try-expand-macro m form)))
              (cond
                ((not result)
                 (loop (cdr ms)))
                ((eq? result *macro-pattern-error*)
                 (display (string-append "error: no matching pattern for ("
                                         (symbol->string (macro-name m))
                                         " ...) with "
                                         (number->string (- (length form) 1))
                                         " argument(s)\n")
                          (current-error-port))
                 (display "  expected one of:\n" (current-error-port))
                 (let ((patterns (macro-patterns m))
                       (nrules (macro-nrules m)))
                   (let ploop ((i 0))
                     (when (< i nrules)
                       (display "  " (current-error-port))
                       (write (vector-ref patterns i) (current-error-port))
                       (newline (current-error-port))
                       (ploop (+ i 1)))))
                 (exit 1))
                (else
                 (expand-one result macros (+ depth 1)))))))))

(define (expand-let-syntax form macros depth)
  (let ((bindings (cadr form))
        (body (cddr form)))
    (if (not (pair? bindings))
        (begin
          (display (string-append "error: " (symbol->string (car form))
                                  " bindings must be a list\n")
                   (current-error-port))
          (exit 1)))

    ;; Parse local macro bindings
    (let ((local-macros
           (map (lambda (b)
                  (if (or (not (pair? b)) (not (= (length b) 2))
                          (not (symbol? (car b))))
                      (begin
                        (display (string-append "error: invalid "
                                                (symbol->string (car form))
                                                " binding\n")
                                 (current-error-port))
                        (exit 1))
                      (let ((m (parse-syntax-rules-binding (car b) (cadr b))))
                        (if (not m)
                            (begin
                              (display (string-append
                                        "error: invalid syntax-rules in "
                                        (symbol->string (car form))
                                        " binding for '"
                                        (symbol->string (car b)) "'\n")
                                       (current-error-port))
                              (exit 1))
                            m))))
                bindings)))

      ;; Combined macro list: local first (highest priority)
      (let* ((combined (append local-macros macros))
             ;; Expand body forms
             (expanded-body (map (lambda (f) (expand-one f combined 0)) body))
             ;; Replace with (begin body...)
             (result (cons 'begin expanded-body)))
        ;; Re-expand
        (expand-one result macros depth)))))

(define (expand-children form macros depth)
  ;; Recurse into all children of a list form
  ;; Zero allocation when nothing changes (all children eq? after expansion)
  (if (not (pair? form))
      form
      (let loop ((fs form))
        (cond
          ((null? fs) form)
          (else
           (let* ((old (car fs))
                  (new (expand-one old macros depth)))
             (if (eq? old new)
                 (loop (cdr fs))
                 ;; Change detected — copy prefix then expand rest
                 (let copy ((src form) (acc '()))
                   (if (eq? src fs)
                       (let rest ((rs (cdr fs)) (acc (cons new acc)))
                         (if (null? rs)
                             (reverse acc)
                             (rest (cdr rs)
                                   (cons (expand-one (car rs) macros depth) acc))))
                       (copy (cdr src) (cons (car src) acc)))))))))))

;;; --- Macro collection (pass 1) ---

(define (collect-macros forms)
  ;; Returns (macros . remaining-forms)
  ;; Macros are collected in reverse order (last defined first)
  ;; so try-macro-expand can iterate directly without reversing
  (let loop ((fs forms) (macros '()) (remaining '()))
    (if (null? fs)
        (cons macros (reverse remaining))
        (let ((form (car fs)))
          (if (is-define-syntax? form)
              (let ((m (parse-syntax-rules form)))
                (if m
                    (loop (cdr fs) (cons m macros) remaining)
                    (loop (cdr fs) macros remaining)))
              (loop (cdr fs) macros (cons form remaining)))))))

;;; --- Public API ---

(define (expand-forms forms)
  ;; Initialize desugar keyword hash table (once)
  (init-desugar-keywords!)
  ;; Reset all state
  (set! *hoisted* '())
  (set! *needs-qq-append* #f)
  (set! *needs-gcd* #f)
  (set! *needs-rationalize* #f)
  (set! *case-counter* 0)
  (set! *or-counter* 0)
  (set! *letrec-counter* 0)
  (set! *do-counter* 0)
  (set! *abs-counter* 0)
  (set! *mod-counter* 0)
  (set! *maxmin-counter* 0)
  (set! *lcm-counter* 0)
  (set! *cwif-counter* 0)
  (set! *foreach-counter* 0)
  (set! *append-counter* 0)
  (set! *receive-counter* 0)
  (set! *string-ctor-counter* 0)
  (set! *promise-counter* 0)
  (set! *macro-counter* 0)

  ;; Pass 1: collect macros
  (let* ((t0 (if *profile* (current-milliseconds) 0))
         (result (collect-macros forms))
         (macros (car result))
         (remaining (cdr result))
         (_ (when *profile* (phase-time "  expand/collect-macros" t0))))

    ;; Pass 2: expand macros and desugar
    (let* ((t0 (if *profile* (current-milliseconds) 0))
           (expanded (map (lambda (f) (expand-one f macros 0)) remaining))
           (_ (when *profile* (phase-time "  expand/expand-one" t0))))

      ;; Hoist helpers
      (if *needs-qq-append*
          (hoist-append-helper!))
      (if *needs-gcd*
          (hoist-gcd-helper!))
      (if *needs-rationalize*
          (hoist-rationalize-helper!))

      ;; Prepend hoisted forms (reversed since add-hoisted! uses cons)
      (append (reverse *hoisted*) expanded))))
