;;; equality.scm — eqv? and equal? runtime functions
;;; These are functions (not constants) because the bodies are
;;; conditionally generated based on which types the program uses.

(define (make-rt-eqv needs-flonum needs-rational needs-complex fn-eqv)
  ;; Build the type-dispatch chain from inside out.
  ;; Only include checks for types the program actually uses.
  (define (build-chain)
    (let* ((base 0)
           (base (if needs-complex
                     `(if (%complex? a)
                          (if (%complex? b)
                              (if (%call ,fn-eqv (%complex-real a) (%complex-real b))
                                  (%call ,fn-eqv (%complex-imag a) (%complex-imag b))
                                  0)
                              0)
                          ,base)
                     base))
           (base (if needs-rational
                     `(if (%rational? a)
                          (if (%rational? b)
                              (%i31-and (%i31-eq (%rational-num a) (%rational-num b))
                                        (%i31-eq (%rational-den a) (%rational-den b)))
                              0)
                          ,base)
                     base))
           (base (if needs-flonum
                     `(if (%flonum? a)
                          (if (%flonum? b)
                              (%f64-eq a b)
                              0)
                          ,base)
                     base)))
      `(if (%ref-eq a b)
           1
           (if (%char? a)
               (if (%char? b)
                   (%i31-eq (%char-code a) (%char-code b))
                   0)
               ,base))))
  (list '("a" "b") '() (list (build-chain))))

(define (make-rt-equal fn-equal)
  (list '("a" "b") '("i" "len" "result")
    (list
      `(if (%ref-eq a b)
           1
           (if (%char? a)
               (if (%char? b)
                   (%i31-eq (%char-code a) (%char-code b))
                   0)
               (if (%pair? a)
                   (if (%pair? b)
                       (if (%call ,fn-equal (car a) (car b))
                           (%call ,fn-equal (cdr a) (cdr b))
                           0)
                       0)
                   (if (%string? a)
                       (if (%string? b)
                           (begin
                             (set! result 1)
                             (set! len (%string-length a))
                             (if (%i31-ne len (%string-length b))
                                 (set! result 0)
                                 (begin
                                   (set! i 0)
                                   (%block-void
                                     (%loop-void
                                       (%br-if 1 (%i31-ge i len))
                                       (%br-if 1 (%i31-eqz result))
                                       (if (%i31-ne (%string-ref a i) (%string-ref b i))
                                           (set! result 0)
                                           0)
                                       (set! i (%i31-add i 1))
                                       (%br 0)))))
                             result)
                           0)
                       0)))))))
