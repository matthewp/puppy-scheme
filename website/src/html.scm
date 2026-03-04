;;; Opcode constructors

(define (open tag)       (list 'open tag))
(define (close-tag)      (list 'close))
(define (attr name val)  (list 'attr name val))
(define (attr-slot name val) (list 'attr-slot name val))
(define (text str)       (list 'text str))
(define (slot str)       (list 'slot str))
(define (event typ name) (list 'event typ name))
(define (component id)   (list 'component id))

;;; html macro — produces nested opcode lists from S-expression HTML

(define-syntax html
  (syntax-rules ()
    ((html node)
     (flatten (html-node node)))))

(define-syntax html-node
  (syntax-rules (@ unquote child)
    ((html-node (unquote expr))
     (list (slot expr)))
    ((html-node (child expr))
     (list (component expr)))
    ((html-node (tag (@ a ...) ch ...))
     (list (open (symbol->string 'tag))
           (html-attrs a ...)
           (html-children ch ...)
           (close-tag)))
    ((html-node (tag ch ...))
     (list (open (symbol->string 'tag))
           (html-children ch ...)
           (close-tag)))
    ((html-node expr)
     (list (text expr)))))

(define-syntax html-attrs
  (syntax-rules (on unquote)
    ((html-attrs) (list))
    ((html-attrs (on type handler) rest ...)
     (list (event type handler) (html-attrs rest ...)))
    ((html-attrs (name (unquote expr)) rest ...)
     (list (attr-slot (symbol->string 'name) expr) (html-attrs rest ...)))
    ((html-attrs (name val) rest ...)
     (list (attr (symbol->string 'name) val) (html-attrs rest ...)))))

(define-syntax html-children
  (syntax-rules ()
    ((html-children) (list))
    ((html-children ch rest ...)
     (list (html-node ch) (html-children rest ...)))))

;;; Flatten nested opcode lists into a flat opcode list.
;;; Opcodes are lists starting with a symbol (open, close, attr, text).
;;; Everything else is a nested list to be flattened.

(define (flatten lst)
  (if (null? lst) '()
      (let ((head (car lst))
            (rest (cdr lst)))
        (if (not (pair? head))
            (flatten rest)
            (if (symbol? (car head))
                (cons head (flatten rest))
                (append (flatten head) (flatten rest)))))))

;;; Render a flat opcode list to an HTML string.

(define void-elements '("meta" "br" "hr" "img" "link" "input"))

(define (render-to-string opcodes)
  (let loop ((ops opcodes) (result "") (tag-stack '()) (in-tag #f))
    (if (null? ops)
        result
        (let ((op (car ops))
              (rest (cdr ops)))
          (let ((type (car op)))
            (if (eq? type 'open)
                (let ((tag (car (cdr op))))
                  (loop rest
                        (string-append result
                          (string-append (if in-tag ">" "")
                            (string-append "<" tag)))
                        (cons tag tag-stack)
                        #t))
            (if (eq? type 'close)
                (if (null? tag-stack)
                    (loop rest result tag-stack #f)
                    (let ((tag (car tag-stack)))
                      (if (member tag void-elements)
                          (loop rest
                                (string-append result (if in-tag ">" ""))
                                (cdr tag-stack)
                                #f)
                          (loop rest
                                (string-append result
                                  (string-append (if in-tag ">" "")
                                    (string-append "</"
                                      (string-append tag ">"))))
                                (cdr tag-stack)
                                #f))))
            (if (eq? type 'attr)
                (let ((name (car (cdr op)))
                      (val (car (cdr (cdr op)))))
                  (loop rest
                        (string-append result
                          (string-append " "
                            (string-append name
                              (string-append "=\""
                                (string-append val "\"")))))
                        tag-stack
                        #t))
            (if (eq? type 'attr-slot)
                (let ((name (car (cdr op)))
                      (val (car (cdr (cdr op)))))
                  (loop rest
                        (string-append result
                          (string-append " "
                            (string-append name
                              (string-append "=\""
                                (string-append val "\"")))))
                        tag-stack
                        #t))
            (if (eq? type 'text)
                (loop rest
                      (string-append result
                        (string-append (if in-tag ">" "") (car (cdr op))))
                      tag-stack
                      #f)
            (if (eq? type 'slot)
                (loop rest
                      (string-append result
                        (string-append (if in-tag ">" "") (car (cdr op))))
                      tag-stack
                      #f)
            (if (eq? type 'event)
                (let ((etype (car (cdr op)))
                      (handler (car (cdr (cdr op)))))
                  (loop rest
                        (string-append result
                          (string-append " data-on-"
                            (string-append etype
                              (string-append "=\""
                                (string-append handler "\"")))))
                        tag-stack
                        #t))
            (if (eq? type 'component)
                (let* ((type-id (car (cdr op)))
                       (instance-id (create-instance type-id))
                       (child-html (render-to-string (render-instance instance-id))))
                  (loop rest
                        (string-append result
                          (string-append (if in-tag ">" "") child-html))
                        tag-stack
                        #f))
                (loop rest result tag-stack in-tag))))))))))))))
