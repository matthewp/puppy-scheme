(define-component counter
  (state (count 0))
  (render
    (html (div (@ (class "counter"))
            (button (@ (on "click" "decrement")) "-")
            (span (@ (class "count")) ,(number->string count))
            (button (@ (on "click" "increment")) "+"))))
  (dispatch event
    (cond ((equal? event "decrement")
           (if (> count 0)
               (set! count (- count 1))))
          ((equal? event "increment")
           (set! count (+ count 1))))))
