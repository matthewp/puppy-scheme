;;; math.scm — Math runtime functions
;;; Polynomial approximations for transcendental functions.
;;; Each function takes eqref (any numeric) and returns flonum.

;; Helper: generate to-f64 conversion for math input (reuse from numbers.scm)
;; (to-f64-expr is already defined in numbers.scm)

;; Helper: Horner polynomial evaluation
;; Generates expression: c[0] + z*(c[1] + z*(...c[n-1]...))
(define (horner-expr z coeffs)
  (let loop ((i (- (length coeffs) 1)))
    (if (= i 0)
        `(%f64-add ,(list-ref coeffs 0) (%f64-mul ,z ,(list-ref coeffs 1)))
        (if (= i (- (length coeffs) 1))
            (list-ref coeffs i)
            `(%f64-add ,(list-ref coeffs i) (%f64-mul ,z ,(loop (+ i 1))))))))

;; Rebuild horner more carefully: c[0] + z*(c[1] + z*(c[2] + ... z*c[n-1]))
(define (horner z coeffs)
  (let ((n (length coeffs)))
    (let loop ((i (- n 2)) (acc (list-ref coeffs (- n 1))))
      (if (< i 0) acc
          (loop (- i 1) `(%f64-add ,(list-ref coeffs i) (%f64-mul ,z ,acc)))))))

;; Kernel sin coefficients
(define S0 -1.66666666666666324348e-01)
(define S1  8.33333333332248946124e-03)
(define S2 -1.98412698298579493134e-04)
(define S3  2.75573137070700676789e-06)
(define S4 -2.50507602534068634195e-08)
(define S5  1.58969099521155010221e-10)

;; Kernel cos coefficients
(define C0  4.16666666666666019037e-02)
(define C1 -1.38888888888741095749e-03)
(define C2  2.48015872894767294178e-05)
(define C3 -2.75573143513906633035e-07)
(define C4  2.08757232129817482790e-09)
(define C5 -1.13596475577881948265e-11)

;; kernel_sin(r, z) = r + r*z*poly(z) where z = r*r
(define (kernel-sin-expr r z)
  (let ((poly (horner z (list S0 S1 S2 S3 S4 S5))))
    `(%f64-add ,r (%f64-mul (%f64-mul ,r ,z) ,poly))))

;; kernel_cos(r, z) = 1 - z/2 + z*z*poly(z)
(define (kernel-cos-expr r z)
  (let ((poly (horner z (list C0 C1 C2 C3 C4 C5))))
    `(%f64-add (%f64-sub 1.0 (%f64-mul ,z 0.5))
               (%f64-mul (%f64-mul ,z ,z) ,poly))))

;; Range reduction for trig: sets r, z, n from x
;; n = nearest(x * 2/pi), r = x - n*pio2_hi - n*pio2_lo, z = r*r, n = n&3
(define trig-reduce-expr
  `(begin
     (set! n (%f64-trunc-to-i31 (%f64-nearest (%f64-mul x 0.6366197723675814))))
     (set! r (%f64-sub (%f64-sub x (%f64-mul (%f64-convert-i31 n) 1.5707963267948966))
                        (%f64-mul (%f64-convert-i31 n) 6.123233995736766e-17)))
     (set! z (%f64-mul r r))
     (set! n (%i31-and n 3))))


;; sqrt: trivial
(define (make-rt-math-sqrt needs-flonum needs-rational)
  (let ((conv (to-f64-expr 'x needs-flonum needs-rational)))
    (list '("x") '()
      (list `(%f64-sqrt ,conv)))))

;; exp: range reduce by ln2, polynomial, scale by 2^k
(define (make-rt-math-exp needs-flonum needs-rational)
  (let ((conv (to-f64-expr 'x needs-flonum needs-rational))
        (P (list 1.66666666666666019037e-01
                 -2.77777777770155933842e-03
                 6.61375632143793436117e-05
                 -1.65339022054652515390e-06
                 4.13813679705723846039e-08)))
    (list '("x") '("xf" "r" "c" "k")
      (list `(begin
               (set! xf ,conv)
               (set! k (%f64-trunc-to-i31 (%f64-nearest (%f64-mul xf 1.4426950408889634))))
               (set! r (%f64-sub (%f64-sub xf
                          (%f64-mul (%f64-convert-i31 k) 6.93147180369123816490e-01))
                          (%f64-mul (%f64-convert-i31 k) 1.90821492927058500170e-10)))
               (set! c (%f64-sub r (%f64-mul (%f64-mul r r)
                          ,(horner '(%f64-mul r r) P))))
               (%f64-mul (%f64-sub 1.0 (%f64-sub (%f64-div (%f64-mul r c)
                                                            (%f64-sub c 2.0))
                                                   r))
                         (%f64-ldexp k)))))))

;; log: extract exponent + mantissa, polynomial
(define (make-rt-math-log needs-flonum needs-rational)
  (let ((conv (to-f64-expr 'x needs-flonum needs-rational))
        (Lg (list 6.666666666666735130e-01
                  3.999999999940941908e-01
                  2.857142874366239149e-01
                  2.222219843214978396e-01
                  1.818357216161805012e-01
                  1.531383769920937332e-01
                  1.479819860511658591e-01)))
    (list '("x") '("xf" "f" "s" "z" "k")
      (list `(begin
               (set! xf ,conv)
               (set! k (%f64-exponent xf))
               (set! f (%f64-sub (%f64-mantissa xf) 1.0))
               (set! s (%f64-div f (%f64-add 2.0 f)))
               (set! z (%f64-mul s s))
               (%f64-add (%f64-mul (%f64-convert-i31 k) 6.931471805599453e-01)
                         (%f64-sub f (%f64-mul s
                           (%f64-sub f (%f64-mul z ,(horner 'z Lg)))))))))))

;; sin
(define (make-rt-math-sin needs-flonum needs-rational)
  (let ((conv (to-f64-expr 'x needs-flonum needs-rational)))
    (list '("x") '("r" "z" "result" "n")
      (list `(begin
               (set! x ,conv)
               ,trig-reduce-expr
               (if (%i31-ne (%i31-and n 1) 0)
                   (set! result ,(kernel-cos-expr 'r 'z))
                   (set! result ,(kernel-sin-expr 'r 'z)))
               (if (%i31-ne (%i31-and n 2) 0)
                   (%f64-neg result)
                   result))))))

;; cos
(define (make-rt-math-cos needs-flonum needs-rational)
  (let ((conv (to-f64-expr 'x needs-flonum needs-rational)))
    (list '("x") '("r" "z" "result" "n")
      (list `(begin
               (set! x ,conv)
               ,trig-reduce-expr
               (if (%i31-ne (%i31-and n 1) 0)
                   (set! result ,(kernel-sin-expr 'r 'z))
                   (set! result ,(kernel-cos-expr 'r 'z)))
               (if (%i31-ne (%i31-and (%i31-add n 1) 2) 0)
                   (%f64-neg result)
                   result))))))

;; tan
(define (make-rt-math-tan needs-flonum needs-rational)
  (let ((conv (to-f64-expr 'x needs-flonum needs-rational))
        (T (list 3.33333333333334091986e-01
                 1.33333333333201242699e-01
                 5.39682539762260521377e-02
                 2.18694882948595424599e-02
                 8.86323982359930005737e-03
                 3.59207910759131235356e-03
                 1.45620945432529025516e-03
                 5.88041240820264096874e-04
                 2.46463134818469906812e-04
                 7.81794442939557092300e-05
                 7.14072491382608190305e-05
                 -1.85586374855275456654e-05
                 2.59073051863633712884e-05)))
    (list '("x") '("r" "z" "t" "n")
      (list `(begin
               (set! x ,conv)
               ,trig-reduce-expr
               (set! t (%f64-add r (%f64-mul (%f64-mul r z)
                          ,(horner 'z T))))
               (if (%i31-ne (%i31-and n 1) 0)
                   (%f64-neg (%f64-div 1.0 t))
                   t))))))

;; asin
(define (make-rt-math-asin needs-flonum needs-rational)
  (let ((conv (to-f64-expr 'x needs-flonum needs-rational))
        (pS (list 1.66666666666666657415e-01
                  -3.25565818622400915405e-01
                  2.01212532134862925881e-01
                  -4.00555345006794114027e-02
                  7.91534994289814532176e-04
                  3.47933107596021167570e-05))
        (qS (list 1.0
                  -2.40339491173441421878e+00
                  2.02094576023350569471e+00
                  -6.88283971605453293030e-01
                  7.70381505559019352791e-02)))
    (list '("x") '("xf" "z" "result" "sign")
      (list `(begin
               (set! xf ,conv)
               (set! sign (%f64-lt xf 0.0))
               (if sign (set! xf (%f64-neg xf)) 0)
               (if (%f64-lt xf 0.5)
                   (begin
                     (set! z (%f64-mul xf xf))
                     (set! result (%f64-add xf
                       (%f64-mul xf (%f64-div (%f64-mul z ,(horner 'z pS))
                                              ,(horner 'z qS))))))
                   (begin
                     (set! z (%f64-div (%f64-sub 1.0 xf) 2.0))
                     (set! xf (%f64-sqrt z))
                     (set! result (%f64-sub 1.5707963267948966
                       (%f64-mul 2.0 (%f64-mul xf
                         (%f64-add 1.0
                           (%f64-div (%f64-mul z ,(horner 'z pS))
                                     ,(horner 'z qS)))))))))
               (if sign (%f64-neg result) result))))))

;; acos
(define (make-rt-math-acos needs-flonum needs-rational)
  (let ((conv (to-f64-expr 'x needs-flonum needs-rational))
        (pS (list 1.66666666666666657415e-01
                  -3.25565818622400915405e-01
                  2.01212532134862925881e-01
                  -4.00555345006794114027e-02
                  7.91534994289814532176e-04
                  3.47933107596021167570e-05))
        (qS (list 1.0
                  -2.40339491173441421878e+00
                  2.02094576023350569471e+00
                  -6.88283971605453293030e-01
                  7.70381505559019352791e-02)))
    (list '("x") '("xf" "z" "result")
      (list `(begin
               (set! xf ,conv)
               (if (%f64-lt (%f64-abs xf) 0.5)
                   (begin
                     (set! z (%f64-mul xf xf))
                     (set! result (%f64-sub 1.5707963267948966
                       (%f64-add xf (%f64-mul xf
                         (%f64-div (%f64-mul z ,(horner 'z pS))
                                   ,(horner 'z qS)))))))
                   (if (%f64-ge xf 0.5)
                       (begin
                         (set! z (%f64-div (%f64-sub 1.0 xf) 2.0))
                         (set! xf (%f64-sqrt z))
                         (set! result (%f64-mul 2.0 (%f64-mul xf
                           (%f64-add 1.0
                             (%f64-div (%f64-mul z ,(horner 'z pS))
                                       ,(horner 'z qS)))))))
                       (begin
                         (set! z (%f64-div (%f64-add 1.0 xf) 2.0))
                         (set! xf (%f64-sqrt z))
                         (set! result (%f64-sub 3.14159265358979323846
                           (%f64-mul 2.0 (%f64-mul xf
                             (%f64-add 1.0
                               (%f64-div (%f64-mul z ,(horner 'z pS))
                                         ,(horner 'z qS))))))))))
               result)))))

;; atan
(define (make-rt-math-atan needs-flonum needs-rational)
  (let ((conv (to-f64-expr 'x needs-flonum needs-rational))
        (aT (list 3.33333333333329318027e-01
                  -1.99999999998764832476e-01
                  1.42857142725034663711e-01
                  -1.11111104054623557880e-01
                  9.09088713343650656196e-02
                  -7.69187620504482999495e-02
                  6.66107313738753120669e-02
                  -5.83357013379057348645e-02
                  4.97687799461593236017e-02
                  -3.65315727442169155270e-02
                  1.62858201153657823623e-02)))
    (list '("x") '("xf" "reduced" "z" "result" "id" "sign")
      (list `(begin
               (set! xf ,conv)
               (set! sign (%f64-lt xf 0.0))
               (if sign (set! xf (%f64-neg xf)) 0)
               (if (%f64-gt xf 2.414213562373095)
                   (begin (set! reduced (%f64-div 1.0 xf))
                          (set! id 3))
                   (if (%f64-gt xf 0.4142135623730950)
                       (begin (set! reduced (%f64-div (%f64-sub xf 1.0)
                                                      (%f64-add xf 1.0)))
                              (set! id 1))
                       (begin (set! reduced xf)
                              (set! id -1))))
               (set! z (%f64-mul reduced reduced))
               (set! result (%f64-sub reduced
                 (%f64-mul reduced (%f64-mul z ,(horner 'z aT)))))
               (if (%i31-eq id 3)
                   (set! result (%f64-add result 1.57079632679489655800e+00))
                   (if (%i31-eq id 1)
                       (set! result (%f64-add result 7.85398163397448278999e-01))
                       0))
               (if sign (%f64-neg result) result))))))

;; atan2(y, x)
(define (make-rt-math-atan2 fn-math-start needs-flonum needs-rational)
  (let ((conv-y (to-f64-expr 'y needs-flonum needs-rational))
        (conv-x (to-f64-expr 'x needs-flonum needs-rational))
        (fn-atan (+ fn-math-start MATH-ATAN)))
    (list '("y" "x") '("yf" "xf" "result" "xneg")
      (list `(begin
               (set! yf ,conv-y)
               (set! xf ,conv-x)
               (set! xneg (%f64-lt xf 0.0))
               (if (%f64-eq xf 0.0)
                   (if (%f64-gt yf 0.0)
                       (set! result 1.5707963267948966)
                       (if (%f64-lt yf 0.0)
                           (set! result -1.5707963267948966)
                           (set! result 0.0)))
                   (begin
                     (set! result (%call ,fn-atan (%f64-div yf xf)))
                     (if xneg
                         (set! result (%f64-add result
                           (%f64-copysign 3.14159265358979323846
                                          (%f64-neg result))))
                         0)))
               result)))))

;; expt(base, power)
(define (make-rt-math-expt fn-flonum-start fn-math-start needs-flonum needs-rational)
  (let ((fl-mul (+ fn-flonum-start FL-MUL))
        (fl-div (+ fn-flonum-start FL-DIV))
        (fn-log (+ fn-math-start MATH-LOG))
        (fn-exp (+ fn-math-start MATH-EXP)))
    (list '("base" "power") '("p" "result" "b")
      (list `(if (%i31? power)
                 (begin
                   (set! p power)
                   (if (%i31-eqz p)
                       1
                       (begin
                         (if (%i31-lt p 0) (set! p (%i31-neg p)) 0)
                         (set! result 1)
                         (set! b base)
                         (%block-void
                           (%loop-void
                             (%br-if 1 (%i31-eqz p))
                             (if (%i31-eqz (%i31-and p 1))
                                 0
                                 (set! result (%call ,fl-mul result b)))
                             (set! p (%i31-div-u p 2))
                             (set! b (%call ,fl-mul b b))
                             (%br 0)))
                         (if (%i31-lt power 0)
                             (%call ,fl-div 1.0 result)
                             result))))
                 (%call ,fn-exp (%call ,fl-mul power (%call ,fn-log base))))))))
