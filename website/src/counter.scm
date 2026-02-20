(include "opcodes.scm")
(include "counter-view.scm")

(define (render)
  (serialize-opcodes (counter-view)))

(define (dispatch-handler ptr len)
  (handle-event (pointer->string ptr len)))

(define (alloc size)
  (linear-alloc 1 size))
