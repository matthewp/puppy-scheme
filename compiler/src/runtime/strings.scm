;;; strings.scm — String runtime functions

(define rt-string-append
  '(("a" "b")                           ;; params
    ("len-a" "len-b" "j" "result")      ;; locals
    ((begin
       (set! len-a (%string-length a))
       (set! len-b (%string-length b))
       (set! result (%make-string (%i31-add len-a len-b) 0))
       (set! j 0)
       (%block-void
         (%loop-void
           (%br-if 1 (%i31-ge j len-a))
           (%string-set! result j (%string-ref a j))
           (set! j (%i31-add j 1))
           (%br 0)))
       (set! j 0)
       (%block-void
         (%loop-void
           (%br-if 1 (%i31-ge j len-b))
           (%string-set! result (%i31-add len-a j) (%string-ref b j))
           (set! j (%i31-add j 1))
           (%br 0)))
       result))))

(define rt-number-to-string
  '(("x")                                     ;; params
    ("n" "is-neg" "len" "tmp" "arr" "idx")     ;; locals
    ((if (%i31-eqz x)
         (%make-string 1 48)
         (begin
           (set! is-neg 0)
           (set! n x)
           (if (%i31-lt n 0)
               (begin (set! is-neg 1) (set! n (%i31-neg n)))
               0)
           (set! len 0)
           (set! tmp n)
           (%block-void
             (%loop-void
               (%br-if 1 (%i31-eqz tmp))
               (set! len (%i31-add len 1))
               (set! tmp (%i31-div-u tmp 10))
               (%br 0)))
           (if (%i31-ne is-neg 0)
               (set! len (%i31-add len 1))
               0)
           (set! arr (%make-string len 0))
           (set! idx (%i31-sub len 1))
           (set! tmp n)
           (%block-void
             (%loop-void
               (%br-if 1 (%i31-eqz tmp))
               (%string-set! arr idx (%i31-add (%i31-rem-u tmp 10) 48))
               (set! idx (%i31-sub idx 1))
               (set! tmp (%i31-div-u tmp 10))
               (%br 0)))
           (if (%i31-ne is-neg 0)
               (%string-set! arr 0 45)
               0)
           arr)))))

(define rt-string-to-number
  '(("x")                                              ;; params
    ("arr" "len" "i" "result" "is-neg" "ch" "valid")   ;; locals
    ((begin
       (set! arr x)
       (set! len (%string-length arr))
       (set! valid 1)
       (set! i 0)
       (set! result 0)
       (set! is-neg 0)
       (if (%i31-eqz len)
           (set! valid 0)
           (begin
             (set! ch (%string-ref arr 0))
             (if (%i31-eq ch 45)
                 (begin (set! is-neg 1) (set! i 1))
                 (if (%i31-eq ch 43)
                     (set! i 1)
                     0))
             (if (%i31-eq i len)
                 (set! valid 0)
                 0)
             (%block-void
               (%loop-void
                 (%br-if 1 (%i31-ge i len))
                 (%br-if 1 (%i31-eqz valid))
                 (set! ch (%string-ref arr i))
                 (if (%i31-lt ch 48)
                     (set! valid 0)
                     (if (%i31-gt ch 57)
                         (set! valid 0)
                         (begin
                           (set! result (%i31-add (%i31-mul result 10)
                                                  (%i31-sub ch 48)))
                           (set! i (%i31-add i 1)))))
                 (%br 0)))))
       (if (%i31-ne valid 0)
           (if (%i31-ne is-neg 0)
               (%i31-neg result)
               result)
           #f)))))

(define rt-substring
  '(("str" "start" "end")
    ("len" "result" "i")
    ((set! len (%i31-sub end start))
     (set! result (%make-string len 0))
     (set! i 0)
     (%block-void
       (%loop-void
         (%br-if 1 (%i31-ge i len))
         (%string-set! result i (%string-ref str (%i31-add start i)))
         (set! i (%i31-add i 1))
         (%br 0)))
     result)))

(define rt-string-copy
  '(("str")
    ("len" "result" "i")
    ((set! len (%string-length str))
     (set! result (%make-string len 0))
     (set! i 0)
     (%block-void
       (%loop-void
         (%br-if 1 (%i31-ge i len))
         (%string-set! result i (%string-ref str i))
         (set! i (%i31-add i 1))
         (%br 0)))
     result)))

(define rt-string-to-list
  '(("str")
    ("len" "i" "result")
    ((set! len (%string-length str))
     (set! result '())
     (set! i (%i31-sub len 1))
     (%block-void
       (%loop-void
         (%br-if 1 (%i31-lt i 0))
         (set! result (cons (%make-char (%string-ref str i)) result))
         (set! i (%i31-sub i 1))
         (%br 0)))
     result)))

(define rt-list-to-string
  '(("lst")
    ("len" "p" "result" "i")
    ((set! len 0)
     (set! p lst)
     (%block-void
       (%loop-void
         (%br-if 1 (null? p))
         (set! len (%i31-add len 1))
         (set! p (cdr p))
         (%br 0)))
     (set! result (%make-string len 0))
     (set! i 0)
     (set! p lst)
     (%block-void
       (%loop-void
         (%br-if 1 (null? p))
         (%string-set! result i (%char-code (car p)))
         (set! i (%i31-add i 1))
         (set! p (cdr p))
         (%br 0)))
     result)))

(define rt-string-eq
  '(("a" "b")
    ("len" "i" "result")
    ((set! len (%string-length a))
     (if (%i31-ne len (%string-length b))
         #f
         (begin
           (set! result 1)
           (set! i 0)
           (%block-void
             (%loop-void
               (%br-if 1 (%i31-ge i len))
               (%br-if 1 (%i31-eqz result))
               (if (%i31-ne (%string-ref a i) (%string-ref b i))
                   (set! result 0)
                   0)
               (set! i (%i31-add i 1))
               (%br 0)))
           (%i31-ne result 0))))))

(define rt-string-lt
  '(("a" "b")
    ("len-a" "len-b" "min-len" "i" "ca" "cb" "done" "result")
    ((set! len-a (%string-length a))
     (set! len-b (%string-length b))
     (set! min-len (if (%i31-lt len-a len-b) len-a len-b))
     (set! i 0)
     (set! done 0)
     (set! result 0)
     (%block-void
       (%loop-void
         (%br-if 1 (%i31-ge i min-len))
         (set! ca (%string-ref a i))
         (set! cb (%string-ref b i))
         (if (%i31-lt ca cb)
             (begin (set! result 1) (set! done 1))
             (if (%i31-gt ca cb)
                 (begin (set! result 0) (set! done 1))
                 0))
         (%br-if 1 done)
         (set! i (%i31-add i 1))
         (%br 0)))
     (if (%i31-ne done 0) (%i31-ne result 0) (%i31-lt len-a len-b)))))

(define rt-string-ci-eq
  '(("a" "b")
    ("len" "i" "result")
    ((set! len (%string-length a))
     (if (%i31-ne len (%string-length b))
         #f
         (begin
           (set! result 1)
           (set! i 0)
           (%block-void
             (%loop-void
               (%br-if 1 (%i31-ge i len))
               (%br-if 1 (%i31-eqz result))
               (if (%i31-ne (%i31-or (%string-ref a i) 32)
                            (%i31-or (%string-ref b i) 32))
                   (set! result 0)
                   0)
               (set! i (%i31-add i 1))
               (%br 0)))
           (%i31-ne result 0))))))

(define rt-string-fill
  '(("s" "ch")
    ("len" "i" "c")
    ((set! c (%char-code ch))
     (set! len (%string-length s))
     (set! i 0)
     (%block-void
       (%loop-void
         (%br-if 1 (%i31-ge i len))
         (%string-set! s i c)
         (set! i (%i31-add i 1))
         (%br 0)))
     0)))

(define rt-string-ci-lt
  '(("a" "b")
    ("len-a" "len-b" "min-len" "i" "ca" "cb" "done" "result")
    ((set! len-a (%string-length a))
     (set! len-b (%string-length b))
     (set! min-len (if (%i31-lt len-a len-b) len-a len-b))
     (set! i 0)
     (set! done 0)
     (set! result 0)
     (%block-void
       (%loop-void
         (%br-if 1 (%i31-ge i min-len))
         (set! ca (%i31-or (%string-ref a i) 32))
         (set! cb (%i31-or (%string-ref b i) 32))
         (if (%i31-lt ca cb)
             (begin (set! result 1) (set! done 1))
             (if (%i31-gt ca cb)
                 (begin (set! result 0) (set! done 1))
                 0))
         (%br-if 1 done)
         (set! i (%i31-add i 1))
         (%br 0)))
     (if (%i31-ne done 0) (%i31-ne result 0) (%i31-lt len-a len-b)))))
