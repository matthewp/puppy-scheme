;; wit: tests/wit/fetch.wit
;; validate-component: true
;; expect: component valid
(define version-text "")
(define version-handle 0)

(define (load)
  (set! version-handle (http-get "https://api.github.com/repos/example/repo/releases/latest")))

(define (deliver-response handle)
  (when (= handle version-handle)
    (set! version-text (response-text handle))))

(define (render-page)
  (string-append "Version: " version-text))
