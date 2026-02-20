;;; Opcode constructors

(define (open tag)       (list 'open tag))
(define (close-tag)      (list 'close))
(define (attr name val)  (list 'attr name val))
(define (text str)       (list 'text str))
(define (slot str)       (list 'slot str))
(define (event typ name) (list 'event typ name))

;;; html macro — produces nested opcode lists from S-expression HTML

(define-syntax html
  (syntax-rules ()
    ((html node)
     (flatten (html-node node)))))

(define-syntax html-node
  (syntax-rules (@ unquote)
    ((html-node (unquote expr))
     (list (slot expr)))
    ((html-node (tag (@ attr ...) child ...))
     (list (open (symbol->string 'tag))
           (html-attrs attr ...)
           (html-children child ...)
           (close-tag)))
    ((html-node (tag child ...))
     (list (open (symbol->string 'tag))
           (html-children child ...)
           (close-tag)))
    ((html-node expr)
     (list (text expr)))))

(define-syntax html-attrs
  (syntax-rules (on)
    ((html-attrs) (list))
    ((html-attrs (on type handler) rest ...)
     (list (event type handler) (html-attrs rest ...)))
    ((html-attrs (name val) rest ...)
     (list (attr (symbol->string 'name) val) (html-attrs rest ...)))))

(define-syntax html-children
  (syntax-rules ()
    ((html-children) (list))
    ((html-children child rest ...)
     (list (html-node child) (html-children rest ...)))))

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
                (loop rest result tag-stack in-tag)))))))))))
