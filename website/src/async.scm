(define-syntax async
  (syntax-rules (await let if when begin)
    ;; let with await in binding
    ((async (let ((var (await (fn args ...)))) body ...))
     (fn args ... (lambda (var) (async (begin body ...)))))
    ;; if — recurse into branches
    ((async (if test then else))
     (if test (async then) (async else)))
    ((async (if test then))
     (if test (async then)))
    ;; when — recurse into body
    ((async (when test body ...))
     (when test (async (begin body ...))))
    ;; begin with let-await as first form
    ((async (begin (let ((var (await (fn args ...)))) let-body ...) rest ...))
     (fn args ... (lambda (var) (async (begin let-body ... rest ...)))))
    ;; begin — single form
    ((async (begin form)) (async form))
    ;; begin — regular form, continue with rest
    ((async (begin form rest ...))
     (begin form (async (begin rest ...))))
    ;; passthrough
    ((async other) other)))
