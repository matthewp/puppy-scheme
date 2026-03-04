;;; numbers.scm — Numeric dispatch runtime functions
;;; Generates dispatch function bodies conditionally based on
;;; which numeric types the program uses.

;; GCD helper: Euclidean algorithm on boxed i31 values
(define rt-gcd-helper
  '(("a" "b") ("tmp")
    ((begin
       (if (%i31-lt a 0) (set! a (%i31-neg a)) 0)
       (if (%i31-lt b 0) (set! b (%i31-neg b)) 0)
       (%block-void
         (%loop-void
           (%br-if 1 (%i31-eqz b))
           (set! tmp b)
           (set! b (%i31-rem-u a b))
           (set! a tmp)
           (%br 0)))
       a))))

;; Helper: generate expression to convert any numeric eqref to flonum
(define (to-f64-expr var needs-flonum needs-rational)
  (cond
    ((and needs-flonum needs-rational)
     `(if (%i31? ,var) (%f64-convert-i31 ,var)
          (if (%flonum? ,var) ,var
              (%f64-div (%f64-convert-i31 (%rational-num ,var))
                        (%f64-convert-i31 (%rational-den ,var))))))
    (needs-flonum
     `(if (%i31? ,var) (%f64-convert-i31 ,var) ,var))
    (needs-rational
     `(if (%i31? ,var) (%f64-convert-i31 ,var)
          (%f64-div (%f64-convert-i31 (%rational-num ,var))
                    (%f64-convert-i31 (%rational-den ,var)))))
    (else `(%f64-convert-i31 ,var))))

;; Helper: generate extract-num/den expression
;; Sets num-var and den-var from eqref (i31 → (val, 1), rational → (field0, field1))
(define (extract-nd-expr var num-var den-var)
  `(if (%i31? ,var)
       (begin (set! ,num-var ,var) (set! ,den-var 1))
       (begin (set! ,num-var (%rational-num ,var))
              (set! ,den-var (%rational-den ,var)))))

;; Helper: normalize rational result in rnum/rden using gcd, return i31 or rational
(define (normalize-rational-expr fn-gcd)
  `(begin
     (set! g (%call ,fn-gcd rnum rden))
     (set! rnum (%i31-div rnum g))
     (set! rden (%i31-div rden g))
     (if (%i31-eq rden 1) rnum (%make-rational rnum rden))))

;; Generate a numeric binop dispatch function
(define (make-rt-numeric-binop i31-op f64-op is-comparison is-division
                                fn-flonum-start fn-gcd fn-make-complex
                                needs-flonum needs-rational needs-complex)

  (define fl-add (+ fn-flonum-start FL-ADD))
  (define fl-sub (+ fn-flonum-start FL-SUB))
  (define fl-mul (+ fn-flonum-start FL-MUL))
  (define fl-div (+ fn-flonum-start FL-DIV))
  (define fl-numeq (+ fn-flonum-start FL-NUM-EQ))

  ;; i31 fast path
  (define fixnum-path
    (if (and is-division needs-rational)
        ;; Division: check if evenly divides, else make rational
        `(if (%i31-eqz (%i31-rem a b))
             (%i31-div a b)
             (begin
               (set! g (%call ,fn-gcd a b))
               (set! rnum (%i31-div a g))
               (set! rden (%i31-div b g))
               (if (%i31-lt rden 0)
                   (begin (set! rnum (%i31-neg rnum))
                          (set! rden (%i31-neg rden)))
                   0)
               (if (%i31-eq rden 1) rnum (%make-rational rnum rden))))
        `(,i31-op a b)))

  ;; Complex path (for a given f64-op)
  (define complex-path
    (if (not needs-complex) #f
        (if (and is-comparison (not (eq? f64-op '%f64-eq)))
            ;; Ordering comparisons on complex → trap
            '(%unreachable)
            (begin
              (let ((extract-a
                     `(if (%complex? a)
                          (begin (set! ar (%complex-real a))
                                 (set! ai (%complex-imag a)))
                          (begin (set! ar a) (set! ai 0))))
                    (extract-b
                     `(if (%complex? b)
                          (begin (set! br (%complex-real b))
                                 (set! bi (%complex-imag b)))
                          (begin (set! br b) (set! bi 0)))))
                (cond
                  ((eq? f64-op '%f64-eq)
                   `(begin ,extract-a ,extract-b
                      (if (%call ,fl-numeq ar br)
                          (%call ,fl-numeq ai bi)
                          #f)))
                  ((or (eq? f64-op '%f64-add) (eq? f64-op '%f64-sub))
                   (let ((fl-off (if (eq? f64-op '%f64-add) fl-add fl-sub)))
                     `(begin ,extract-a ,extract-b
                        (%call ,fn-make-complex
                               (%call ,fl-off ar br)
                               (%call ,fl-off ai bi)))))
                  ((eq? f64-op '%f64-mul)
                   ;; (a+bi)(c+di) = (ac-bd)+(ad+bc)i
                   `(begin ,extract-a ,extract-b
                      (%call ,fn-make-complex
                             (%call ,fl-sub (%call ,fl-mul ar br)
                                            (%call ,fl-mul ai bi))
                             (%call ,fl-add (%call ,fl-mul ar bi)
                                            (%call ,fl-mul ai br)))))
                  ((eq? f64-op '%f64-div)
                   ;; (a+bi)/(c+di) = ((ac+bd)+(bc-ad)i)/(c²+d²)
                   `(begin ,extract-a ,extract-b
                      (set! denom (%call ,fl-add
                                         (%call ,fl-mul br br)
                                         (%call ,fl-mul bi bi)))
                      (%call ,fn-make-complex
                             (%call ,fl-div
                                    (%call ,fl-add (%call ,fl-mul ar br)
                                                   (%call ,fl-mul ai bi))
                                    denom)
                             (%call ,fl-div
                                    (%call ,fl-sub (%call ,fl-mul ai br)
                                                   (%call ,fl-mul ar bi))
                                    denom))))
                  (else '(%unreachable))))))))

  ;; Flonum path
  (define flonum-path
    (if (not needs-flonum) #f
        (let ((fa (to-f64-expr 'a needs-flonum needs-rational))
              (fb (to-f64-expr 'b needs-flonum needs-rational)))
          (if is-comparison
              `(begin (set! fa ,fa) (set! fb ,fb) (,f64-op fa fb))
              `(begin (set! fa ,fa) (set! fb ,fb) (,f64-op fa fb))))))

  ;; Rational path
  (define rational-path
    (if (not needs-rational) #f
        (begin
          (let ((extract `(begin ,(extract-nd-expr 'a 'anum 'aden)
                                 ,(extract-nd-expr 'b 'bnum 'bden))))
            (if is-comparison
                ;; ad vs bc
                (let ((cmp-i31-op (cond ((eq? f64-op '%f64-eq) '%i31-eq)
                                        ((eq? f64-op '%f64-lt) '%i31-lt)
                                        ((eq? f64-op '%f64-gt) '%i31-gt)
                                        ((eq? f64-op '%f64-le) '%i31-le)
                                        ((eq? f64-op '%f64-ge) '%i31-ge)
                                        (else '%i31-eq))))
                  `(begin ,extract (,cmp-i31-op (%i31-mul anum bden)
                                                (%i31-mul bnum aden))))
                (if is-division
                    ;; (a/b)/(c/d) = ad/bc
                    `(begin ,extract
                       (set! rnum (%i31-mul anum bden))
                       (set! rden (%i31-mul aden bnum))
                       (if (%i31-lt rden 0)
                           (begin (set! rnum (%i31-neg rnum))
                                  (set! rden (%i31-neg rden)))
                           0)
                       ,(normalize-rational-expr fn-gcd))
                    ;; Arithmetic: +, -, *
                    (cond
                      ((eq? f64-op '%f64-add)
                       `(begin ,extract
                          (set! rnum (%i31-add (%i31-mul anum bden)
                                               (%i31-mul bnum aden)))
                          (set! rden (%i31-mul aden bden))
                          ,(normalize-rational-expr fn-gcd)))
                      ((eq? f64-op '%f64-sub)
                       `(begin ,extract
                          (set! rnum (%i31-sub (%i31-mul anum bden)
                                               (%i31-mul bnum aden)))
                          (set! rden (%i31-mul aden bden))
                          ,(normalize-rational-expr fn-gcd)))
                      ((eq? f64-op '%f64-mul)
                       `(begin ,extract
                          (set! rnum (%i31-mul anum bnum))
                          (set! rden (%i31-mul aden bden))
                          ,(normalize-rational-expr fn-gcd)))
                      (else '(%unreachable)))))))))

  ;; Build slow path (inside-out: complex wraps flonum wraps rational)
  (define slow-path
    (let* ((base (cond
                   ((and needs-flonum needs-rational)
                    `(if (if (%flonum? a) #t (%flonum? b))
                         ,flonum-path
                         ,rational-path))
                   (needs-flonum flonum-path)
                   (needs-rational rational-path)
                   (else '(%unreachable)))))
      (if needs-complex
          `(if (if (%complex? a) #t (%complex? b))
               ,complex-path
               ,base)
          base)))

  ;; Extra locals needed
  (define extra-locals
    (let ((ls '()))
      (when needs-rational
        (set! ls (append ls '("anum" "aden" "bnum" "bden" "rnum" "rden" "g"))))
      (when needs-complex
        (set! ls (append ls '("ar" "ai" "br" "bi" "denom"))))
      (when needs-flonum
        (set! ls (append ls '("fa" "fb"))))
      ls))

  (list '("a" "b") extra-locals
    (list `(if (if (%i31? a) (%i31? b) #f)
               ,fixnum-path
               ,slow-path))))


;; exact->inexact
(define (make-rt-exact-to-inexact fn-flonum-start fn-make-complex
                                   needs-flonum needs-rational needs-complex)
  (if (not needs-flonum)
      '(("x") () (x))
      (let ((fl-e2i (+ fn-flonum-start FL-EXACT-TO-INEXACT))
            (core `(if (%i31? x)
                       (%f64-convert-i31 x)
                       ,(if needs-rational
                            `(if (%rational? x)
                                 (%f64-div (%f64-convert-i31 (%rational-num x))
                                           (%f64-convert-i31 (%rational-den x)))
                                 x)
                            'x))))
        (if needs-complex
            (list '("x") '()
              (list `(if (%complex? x)
                         (%call ,fn-make-complex
                                (%call ,fl-e2i (%complex-real x))
                                (%call ,fl-e2i (%complex-imag x)))
                         ,core)))
            (list '("x") '() (list core))))))

;; inexact->exact
(define (make-rt-inexact-to-exact fn-flonum-start fn-make-complex
                                   needs-flonum needs-rational needs-complex)
  (let ((fl-i2e (+ fn-flonum-start FL-INEXACT-TO-EXACT))
        (core `(if (%i31? x)
                   x
                   ,(if needs-flonum
                        '(%f64-trunc-to-i31 x)
                        'x))))
    (if needs-complex
        (list '("x") '()
          (list `(if (%complex? x)
                     (%call ,fn-make-complex
                            (%call ,fl-i2e (%complex-real x))
                            (%call ,fl-i2e (%complex-imag x)))
                     ,core)))
        (list '("x") '() (list core)))))

;; Rounding ops (floor, ceiling, truncate, round)
(define (make-rt-round-op f64-round-op
                           needs-flonum needs-rational needs-complex)
  (let* ((complex-trap
           (if needs-complex
               `(if (%complex? x) (%unreachable) 0)
               0))
         (flonum-path
           (if needs-flonum `(,f64-round-op x) 'x))
         (else-path
           (if needs-rational
               `(if (%rational? x)
                    (%i31-div (%rational-num x) (%rational-den x))
                    ,flonum-path)
               flonum-path)))
    (list '("x") '()
      (list `(begin ,complex-trap
                    (if (%i31? x) x ,else-path))))))
