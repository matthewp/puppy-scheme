(define (cache-get url callback)
  (let ((handle (raw-cache-get url)))
    (register-callback handle callback)))

(define (cache-hit? handle)
  (= (raw-cache-hit handle) 1))

(define (cache-put url handle ttl)
  (raw-cache-put url handle ttl))
