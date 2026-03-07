(define (http-fetch url callback)
  (let ((handle (raw-http-get url)))
    (register-callback handle callback)))

(define (fetch-text handle callback)
  (let ((text-handle (raw-response-text handle)))
    (register-callback text-handle callback)))

(define (fetch-status handle)
  (raw-response-status handle))

(define (fetch-string handle)
  (raw-get-string handle))

(define (fetch-json-get json key)
  (raw-json-get json key))
