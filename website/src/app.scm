(import (puppykit ui))

(include "app/hello.scm")
(include "app/counter.scm")
(include "app/tabs.scm")

;;; ABI exports

(define (create type-id)
  (create-instance type-id))

(define (render instance-id)
  (serialize-opcodes (render-instance instance-id)))

(define (dispatch instance-id event-ptr event-len)
  (dispatch-instance instance-id (pointer->string event-ptr event-len)))

(define (alloc size)
  (linear-alloc 1 size))

(define (root) tabs)
