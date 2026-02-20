;;; Shared counter state and view — included by counter.scm and homepage.scm

(define count 0)

(define (counter-view)
  (html (div (@ (class "counter"))
          (button (@ (on "click" "on_decrement")) "-")
          (span (@ (class "count")) ,(number->string count))
          (button (@ (on "click" "on_increment")) "+"))))

(define (handle-event handler)
  (cond ((equal? handler "on_decrement")
         (if (> count 0)
             (set! count (- count 1))))
        ((equal? handler "on_increment")
         (set! count (+ count 1)))))
