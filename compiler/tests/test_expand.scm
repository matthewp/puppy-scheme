;;; test_expand.scm — Unit tests for the expander

(include "../src/compiler/macro.scm")
(include "../src/compiler/expand.scm")

;;; --- Test harness ---

(define *pass* 0)
(define *fail* 0)
(define *skip* 0)

(define (check name expected actual)
  (if (equal? expected actual)
      (set! *pass* (+ *pass* 1))
      (begin
        (set! *fail* (+ *fail* 1))
        (display "FAIL: ")
        (display name)
        (newline)
        (display "  expected: ")
        (write expected)
        (newline)
        (display "  actual:   ")
        (write actual)
        (newline))))

(define (skip name reason)
  (set! *skip* (+ *skip* 1))
  (display "SKIP: ")
  (display name)
  (display " — ")
  (display reason)
  (newline))

(define (report)
  (newline)
  (display *pass*)
  (display " passed, ")
  (display *fail*)
  (display " failed, ")
  (display *skip*)
  (display " skipped")
  (newline)
  (if (> *fail* 0) (exit 1)))

;;; --- filter-renames ---

(check "filter-renames: empty renames"
       '()
       (filter-renames '() '(x y)))

(check "filter-renames: no shadows"
       '((a . a1) (b . b1))
       (filter-renames '((a . a1) (b . b1)) '()))

(check "filter-renames: shadow one"
       '((b . b1))
       (filter-renames '((a . a1) (b . b1)) '(a)))

(check "filter-renames: shadow all"
       '()
       (filter-renames '((a . a1) (b . b1)) '(a b)))

(check "filter-renames: shadow not in renames"
       '((a . a1))
       (filter-renames '((a . a1)) '(z)))

;;; --- rename-free ---

(check "rename-free: symbol hit"
       'y
       (rename-free 'x '((x . y))))

(check "rename-free: symbol miss"
       'z
       (rename-free 'z '((x . y))))

(check "rename-free: non-symbol atom"
       42
       (rename-free 42 '((x . y))))

(check "rename-free: empty renames"
       '(x y z)
       (rename-free '(x y z) '()))

(check "rename-free: simple list"
       '(y z)
       (rename-free '(x z) '((x . y))))

(check "rename-free: quote is opaque"
       '(quote (x y))
       (rename-free '(quote (x y)) '((x . renamed))))

(check "rename-free: lambda shadows params"
       '(lambda (x) x)
       (rename-free '(lambda (x) x) '((x . renamed))))

(check "rename-free: lambda renames free vars"
       '(lambda (x) renamed)
       (rename-free '(lambda (x) y) '((y . renamed))))

(check "rename-free: let shadows bindings"
       '(let ((x (renamed))) x)
       (rename-free '(let ((x (y))) x) '((y . renamed) (x . bad))))

(check "rename-free: let* progressive shadowing"
       '(let* ((x (renamed)) (y x)) y)
       (rename-free '(let* ((x (z)) (y x)) y) '((z . renamed))))

(check "rename-free: define renames free, shadows params"
       '(define (f x) renamed)
       (rename-free '(define (f x) y) '((y . renamed) (x . bad))))

;;; --- desugar-named-let ---

(let ()
  (set! *letrec-counter* 0)
  (let ((result (desugar-named-let '(let loop ((i 0) (acc 1))
                                      (if (= i 0) acc
                                          (loop (- i 1) (* acc i)))))))
    (check "desugar-named-let: produces letrec"
           'letrec
           (car result))
    (check "desugar-named-let: binding name is loop"
           'loop
           (caar (cadr result)))
    (check "desugar-named-let: lambda params"
           '(i acc)
           (cadr (cadar (cadr result))))
    (check "desugar-named-let: initial call"
           '(loop 0 1)
           (caddr result))))

;;; --- desugar-letrec ---

(let ()
  (set! *letrec-counter* 0)
  (let ((result (desugar-letrec '(letrec ((x 10) (y 20)) (+ x y)))))
    (check "desugar-letrec: produces let"
           'let
           (car result))
    (check "desugar-letrec: let bindings init to 0"
           '((__lr0_x 0) (__lr0_y 0))
           (cadr result))
    (check "desugar-letrec: set! x"
           '(set! __lr0_x 10)
           (caddr result))
    (check "desugar-letrec: set! y"
           '(set! __lr0_y 20)
           (cadddr result))
    (check "desugar-letrec: body renamed"
           '(+ __lr0_x __lr0_y)
           (car (cddddr result)))))

;;; --- Composed: named-let → letrec → desugared ---

(let ()
  (set! *letrec-counter* 0)
  (let* ((step1 (desugar-named-let '(let loop ((i 0)) (loop (+ i 1)))))
         (step2 (desugar-letrec step1)))
    (check "named-let→letrec: step1 is letrec"
           'letrec
           (car step1))
    (check "named-let→letrec: step2 is let"
           'let
           (car step2))
    (check "named-let→letrec: renamed var in let"
           '__lr0_loop
           (caar (cadr step2)))
    (check "named-let→letrec: init call renamed"
           '(__lr0_loop 0)
           (car (cdddr step2)))))

;;; --- desugar-case ---

(let ()
  (set! *case-counter* 0)
  (let ((result (desugar-case '(case x ((1) "one") ((2 3) "two-three") (else "other")))))
    (check "desugar-case: produces let"
           'let
           (car result))
    (check "desugar-case: key binding"
           '((__k0 x))
           (cadr result))
    (check "desugar-case: body is cond"
           'cond
           (caaddr result))
    ;; First clause: single datum
    (check "desugar-case: single datum test"
           '(eq? __k0 1)
           (car (cadr (caddr result))))
    ;; Second clause: multiple datums
    (check "desugar-case: multi datum test"
           '(or (eq? __k0 2) (eq? __k0 3))
           (car (caddr (caddr result))))
    ;; Else clause preserved
    (check "desugar-case: else clause"
           'else
           (car (cadddr (caddr result))))))

;;; --- desugar-do ---

(let ()
  (set! *do-counter* 0)
  (let ((result (desugar-do '(do ((i 0 (+ i 1))) ((= i 10) i)))))
    (check "desugar-do: produces letrec"
           'letrec
           (car result))
    (check "desugar-do: loop name"
           '__do0
           (caar (cadr result)))
    (check "desugar-do: lambda params"
           '(i)
           (cadr (cadar (cadr result))))
    (check "desugar-do: initial call"
           '(__do0 0)
           (caddr result))))

;;; --- desugar-internal-defines ---

(check "desugar-internal-defines: no defines"
       '((+ 1 2))
       (desugar-internal-defines '((+ 1 2))))

(check "desugar-internal-defines: single define"
       '((letrec ((x 10)) (+ x 1)))
       (desugar-internal-defines '((define x 10) (+ x 1))))

(check "desugar-internal-defines: function define"
       '((letrec ((f (lambda (x) (* x x)))) (f 5)))
       (desugar-internal-defines '((define (f x) (* x x)) (f 5))))

(check "desugar-internal-defines: multiple defines"
       '((letrec ((x 1) (y 2)) (+ x y)))
       (desugar-internal-defines '((define x 1) (define y 2) (+ x y))))

;;; --- desugar-cxxr ---

(check "desugar-cxxr: cadr"
       '(car (cdr x))
       (desugar-cxxr 'cadr '(cadr x)))

(check "desugar-cxxr: caddr"
       '(car (cdr (cdr x)))
       (desugar-cxxr 'caddr '(caddr x)))

(check "desugar-cxxr: cdar"
       '(cdr (car x))
       (desugar-cxxr 'cdar '(cdar x)))

(check "desugar-cxxr: cddr"
       '(cdr (cdr x))
       (desugar-cxxr 'cddr '(cddr x)))

(check "desugar-cxxr: caar"
       '(car (car x))
       (desugar-cxxr 'caar '(caar x)))

(check "desugar-cxxr: cdddr"
       '(cdr (cdr (cdr x)))
       (desugar-cxxr 'cdddr '(cdddr x)))

;;; --- expand-qq ---

(check "expand-qq: literal symbol"
       '(quote foo)
       (expand-qq 'foo))

(check "expand-qq: literal number"
       '(quote 42)
       (expand-qq 42))

(check "expand-qq: unquote"
       'x
       (expand-qq '(unquote x)))

(check "expand-qq: simple list"
       '(cons (quote a) (cons (quote b) (quote ())))
       (expand-qq '(a b)))

(check "expand-qq: list with unquote"
       '(cons (quote a) (cons x (quote ())))
       (expand-qq '(a (unquote x))))

(check "expand-qq: unquote-splicing"
       (begin (set! *needs-qq-append* #f)
              (let ((r (expand-qq '(a (unquote-splicing xs)))))
                (set! *needs-qq-append* #f)
                r))
       '(cons (quote a) (__qq_append xs (quote ()))))

(check "expand-qq: empty list"
       '(quote ())
       (expand-qq '()))

;;; --- expand-forms: full pipeline ---

;; Simple expression — no transformation
(check "expand-forms: literal"
       '(42)
       (expand-forms '(42)))

(check "expand-forms: simple call"
       '((+ 1 2))
       (expand-forms '((+ 1 2))))

;; let → unchanged (not named-let, not letrec)
(check "expand-forms: plain let"
       '((let ((x 1)) x))
       (expand-forms '((let ((x 1)) x))))

;; named-let → desugared through letrec
(let ()
  (let ((result (expand-forms '((let loop ((i 0)) (if (= i 5) i (loop (+ i 1))))))))
    (check "expand-forms: named-let produces let"
           'let
           (caar result))
    ;; Should have renamed vars like __lr0_loop
    (check "expand-forms: named-let has renamed binding"
           '__lr0_loop
           (caar (cadar result)))))

;; letrec → desugared
(let ()
  (let ((result (expand-forms '((letrec ((x 10)) (+ x 1))))))
    (check "expand-forms: letrec becomes let"
           'let
           (caar result))
    (check "expand-forms: letrec renamed var"
           '__lr0_x
           (caar (cadar result)))))

;; cond, and, or pass through (desugared later in codegen)
(check "expand-forms: cond passes through"
       '((cond (#t 1)))
       (expand-forms '((cond (#t 1)))))

(check "expand-forms: and passes through"
       '((and x y))
       (expand-forms '((and x y))))

(check "expand-forms: or passes through"
       '((or x y))
       (expand-forms '((or x y))))

;; when → if + begin
(let ()
  (let ((result (expand-forms '((when #t 1 2)))))
    (check "expand-forms: when becomes if"
           'if
           (caar result))))

;; quasiquote
(let ()
  (let ((result (expand-forms '((quasiquote (a b))))))
    (check "expand-forms: quasiquote produces cons"
           'cons
           (caar result))))

;; define-syntax + usage
(let ()
  (let ((result (expand-forms '((define-syntax my-if
                                  (syntax-rules ()
                                    ((my-if c t f) (if c t f))))
                                (my-if #t 1 2)))))
    (check "expand-forms: macro expansion"
           '(if #t 1 2)
           (car result))))

;; n-ary arithmetic
(check "expand-forms: ternary +"
       '((+ (+ 1 2) 3))
       (expand-forms '((+ 1 2 3))))

(check "expand-forms: quaternary +"
       '((+ (+ (+ 1 2) 3) 4))
       (expand-forms '((+ 1 2 3 4))))

;; unary minus
(check "expand-forms: unary minus"
       '((- 0 x))
       (expand-forms '((- x))))

;; do → letrec → let
(let ()
  (let ((result (expand-forms '((do ((i 0 (+ i 1))) ((= i 5) i))))))
    (check "expand-forms: do produces let (via letrec)"
           'let
           (caar result))))

;; internal defines → letrec → let
(let ()
  (let ((result (expand-forms '((lambda () (define x 1) (+ x 2))))))
    (check "expand-forms: internal define in lambda"
           'lambda
           (caar result))
    ;; Body should contain a let (from desugared letrec)
    (check "expand-forms: internal define becomes let"
           'let
           (caaddr (car result)))))

;;; --- desugar-abs ---

(let ()
  (set! *abs-counter* 0)
  (let ((result (desugar-abs '(abs x))))
    (check "desugar-abs: produces let"
           'let
           (car result))
    (check "desugar-abs: tmp binding"
           '((__abs0 x))
           (cadr result))
    (check "desugar-abs: if negative"
           'if
           (caaddr result))))

;;; --- desugar-modulo ---

(let ()
  (set! *mod-counter* 0)
  (let ((result (desugar-modulo '(modulo a b))))
    (check "desugar-modulo: produces let"
           'let
           (car result))
    (check "desugar-modulo: binds a and b"
           '((__mod0_a a) (__mod0_b b))
           (cadr result))))

;;; --- desugar-for-each ---

(let ()
  (set! *foreach-counter* 0)
  (let ((result (desugar-for-each '(for-each f lst))))
    (check "desugar-for-each: named let form"
           'let
           (car result))
    (check "desugar-for-each: loop name"
           '__foreach0
           (cadr result))
    (check "desugar-for-each: binding"
           '((l lst))
           (caddr result))))

;;; --- desugar-reverse ---

(let ()
  (set! *reverse-counter* 0)
  (let ((result (desugar-reverse '(reverse xs))))
    (check "desugar-reverse: named let"
           'let
           (car result))
    (check "desugar-reverse: loop name"
           '__reverse0
           (cadr result))))

;;; --- desugar-length ---

(let ()
  (set! *length-counter* 0)
  (let ((result (desugar-length '(length xs))))
    (check "desugar-length: named let"
           'let
           (car result))
    (check "desugar-length: loop name"
           '__length0
           (cadr result))
    (check "desugar-length: bindings"
           '((l xs) (n 0))
           (caddr result))))

;;; --- desugar-map ---

(let ()
  (set! *map-counter* 0)
  (let ((result (desugar-map '(map f xs))))
    (check "desugar-map: named let"
           'let
           (car result))
    (check "desugar-map: loop name"
           '__map0
           (cadr result))))

;;; --- desugar-receive ---

(let ()
  (set! *receive-counter* 0)
  (let ((result (desugar-receive '(receive (x y) expr body1 body2))))
    (check "desugar-receive: outer let"
           'let
           (car result))
    (check "desugar-receive: tmp name"
           '__recv0
           (caar (cadr result)))
    (check "desugar-receive: tmp bound to expr"
           'expr
           (cadar (cadr result)))
    ;; Inner let destructures
    (check "desugar-receive: inner let"
           'let
           (caaddr result))
    (check "desugar-receive: x accessor"
           '(car __recv0)
           (cadar (cadr (caddr result))))
    (check "desugar-receive: y accessor"
           '(car (cdr __recv0))
           (cadadr (cadr (caddr result))))))

;;; --- Done ---

(report)
