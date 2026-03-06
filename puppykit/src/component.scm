;;; Component system — type registry and instance management

;; Type registry — vector of factory functions
;; Each factory returns (cons render-fn dispatch-fn)
(define *component-types* (make-vector 16 #f))
(define *next-type* 0)

(define (register-component! factory)
  (let ((id *next-type*))
    (vector-set! *component-types* id factory)
    (set! *next-type* (+ *next-type* 1))
    id))

;; Instance table — vector of (type-id . (cons render-fn dispatch-fn))
(define *instances* (make-vector 256 #f))
(define *next-id* 0)

(define (create-instance type-id)
  (let* ((factory (vector-ref *component-types* type-id))
         (pair (factory))
         (id *next-id*))
    (vector-set! *instances* id (cons type-id pair))
    (set! *next-id* (+ *next-id* 1))
    id))

(define (render-instance instance-id)
  (let* ((entry (vector-ref *instances* instance-id))
         (pair (cdr entry))
         (render-fn (car pair)))
    (render-fn)))

(define (dispatch-instance instance-id event)
  (let* ((entry (vector-ref *instances* instance-id))
         (pair (cdr entry))
         (dispatch-fn (cdr pair)))
    (dispatch-fn event)))

(define-syntax define-component
  (syntax-rules (state render dispatch)
    ;; render only
    ((define-component name (render rbody))
     (define name
       (register-component!
         (lambda ()
           (cons (lambda () rbody)
                 (lambda (event) #f))))))
    ;; render + dispatch
    ((define-component name (render rbody) (dispatch evt dbody ...))
     (define name
       (register-component!
         (lambda ()
           (cons (lambda () rbody)
                 (lambda (evt) dbody ...))))))
    ;; state (1 binding) + render
    ((define-component name (state (v1 i1)) (render rbody))
     (define name
       (register-component!
         (lambda ()
           (let ((v1 i1))
             (cons (lambda () rbody)
                   (lambda (event) #f)))))))
    ;; state (2 bindings) + render
    ((define-component name (state (v1 i1) (v2 i2)) (render rbody))
     (define name
       (register-component!
         (lambda ()
           (let ((v1 i1) (v2 i2))
             (cons (lambda () rbody)
                   (lambda (event) #f)))))))
    ;; state (1 binding) + render + dispatch
    ((define-component name (state (v1 i1)) (render rbody) (dispatch evt dbody ...))
     (define name
       (register-component!
         (lambda ()
           (let ((v1 i1))
             (cons (lambda () rbody)
                   (lambda (evt) dbody ...)))))))
    ;; state (2 bindings) + render + dispatch
    ((define-component name (state (v1 i1) (v2 i2)) (render rbody) (dispatch evt dbody ...))
     (define name
       (register-component!
         (lambda ()
           (let ((v1 i1) (v2 i2))
             (cons (lambda () rbody)
                   (lambda (evt) dbody ...)))))))))
