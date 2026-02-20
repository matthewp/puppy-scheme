;; expect: 21
(define-syntax swap!
  (syntax-rules ()
    ((swap! a b)
     (let ((tmp a))
       (set! a b)
       (set! b tmp)))))
(define tmp 1)
(define y 2)
(swap! tmp y)
(display tmp)
(display y)
