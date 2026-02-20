;;; vectors.scm — Vector runtime helpers

(define rt-vector-copy
  '(("v")
    ("len" "result" "i")
    ((set! len (%vector-length v))
     (set! result (%make-vector '() len))
     (set! i 0)
     (%block-void
       (%loop-void
         (%br-if 1 (%i31-ge i len))
         (%vector-set! result i (%vector-ref v i))
         (set! i (%i31-add i 1))
         (%br 0)))
     result)))

(define rt-list-to-vector
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
     (set! result (%make-vector '() len))
     (set! i 0)
     (set! p lst)
     (%block-void
       (%loop-void
         (%br-if 1 (null? p))
         (%vector-set! result i (car p))
         (set! i (%i31-add i 1))
         (set! p (cdr p))
         (%br 0)))
     result)))

(define rt-vector-to-list
  '(("v")
    ("len" "i" "result")
    ((set! len (%vector-length v))
     (set! result '())
     (set! i (%i31-sub len 1))
     (%block-void
       (%loop-void
         (%br-if 1 (%i31-lt i 0))
         (set! result (cons (%vector-ref v i) result))
         (set! i (%i31-sub i 1))
         (%br 0)))
     result)))
