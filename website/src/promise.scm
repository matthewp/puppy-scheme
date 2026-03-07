(define *promise-callbacks* '())

(define (register-callback handle callback)
  (set! *promise-callbacks* (cons (cons handle callback) *promise-callbacks*)))

(define (deliver-promise-result handle)
  (let ((cb #f))
    ;; Find and remove the callback first
    (set! *promise-callbacks*
      (let loop ((cbs *promise-callbacks*) (acc '()))
        (cond
          ((null? cbs) acc)
          ((= (caar cbs) handle)
           (set! cb (cdar cbs))
           (loop (cdr cbs) acc))
          (else (loop (cdr cbs) (cons (car cbs) acc))))))
    ;; Call after removal so new registrations aren't overwritten
    (when cb (cb handle))))
