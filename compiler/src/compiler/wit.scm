;;; wit.scm — WIT (WebAssembly Interface Type) parser
;;; Parses .wit files into structured data for Component Model support.

;;; --- Data structure constructors and accessors ---

;; Package: #(namespace name version interfaces worlds)
(define (make-wit-package ns name ver ifaces worlds)
  (vector ns name ver ifaces worlds))
(define (wit-package-namespace p) (vector-ref p 0))
(define (wit-package-name p) (vector-ref p 1))
(define (wit-package-version p) (vector-ref p 2))
(define (wit-package-interfaces p) (vector-ref p 3))
(define (wit-package-worlds p) (vector-ref p 4))

;; Interface: #(name items gate)
(define (make-wit-interface name items gate)
  (vector name items gate))
(define (wit-interface-name i) (vector-ref i 0))
(define (wit-interface-items i) (vector-ref i 1))
(define (wit-interface-gate i) (vector-ref i 2))

;; World: #(name items gate)
(define (make-wit-world name items gate)
  (vector name items gate))
(define (wit-world-name w) (vector-ref w 0))
(define (wit-world-items w) (vector-ref w 1))
(define (wit-world-gate w) (vector-ref w 2))

;; Function: #(name params result gate)
(define (make-wit-func name params result gate)
  (vector name params result gate))
(define (wit-func-name f) (vector-ref f 0))
(define (wit-func-params f) (vector-ref f 1))
(define (wit-func-result f) (vector-ref f 2))
(define (wit-func-gate f) (vector-ref f 3))

;; Record: #(name fields gate)  fields: list of (name . type)
(define (make-wit-record name fields gate)
  (vector name fields gate))
(define (wit-record-name r) (vector-ref r 0))
(define (wit-record-fields r) (vector-ref r 1))
(define (wit-record-gate r) (vector-ref r 2))

;; Variant: #(name cases gate)  cases: list of (name . type-or-#f)
(define (make-wit-variant name cases gate)
  (vector name cases gate))
(define (wit-variant-name v) (vector-ref v 0))
(define (wit-variant-cases v) (vector-ref v 1))
(define (wit-variant-gate v) (vector-ref v 2))

;; Enum: #(name cases gate)  cases: list of strings
(define (make-wit-enum name cases gate)
  (vector name cases gate))
(define (wit-enum-name e) (vector-ref e 0))
(define (wit-enum-cases e) (vector-ref e 1))
(define (wit-enum-gate e) (vector-ref e 2))

;; Flags: #(name fields gate)  fields: list of strings
(define (make-wit-flags name fields gate)
  (vector name fields gate))
(define (wit-flags-name f) (vector-ref f 0))
(define (wit-flags-fields f) (vector-ref f 1))
(define (wit-flags-gate f) (vector-ref f 2))

;; Resource: #(name methods gate)  methods: list of functions
(define (make-wit-resource name methods gate)
  (vector name methods gate))
(define (wit-resource-name r) (vector-ref r 0))
(define (wit-resource-methods r) (vector-ref r 1))
(define (wit-resource-gate r) (vector-ref r 2))

;; Use: #(path names gate)  names: list of (name . alias-or-#f)
(define (make-wit-use path names gate)
  (vector path names gate))
(define (wit-use-path u) (vector-ref u 0))
(define (wit-use-names u) (vector-ref u 1))
(define (wit-use-gate u) (vector-ref u 2))

;; TypeAlias: #(name type gate)
(define (make-wit-type-alias name ty gate)
  (vector name ty gate))
(define (wit-type-alias-name a) (vector-ref a 0))
(define (wit-type-alias-type a) (vector-ref a 1))
(define (wit-type-alias-gate a) (vector-ref a 2))

;;; --- Keyword hash table ---

(define *wit-keywords* (make-string-ht))

(define (init-wit-keywords!)
  (let ((kws '("as" "bool" "borrow" "char" "constructor" "enum" "export"
                "f32" "f64" "flags" "from" "func" "future" "import"
                "include" "interface" "list" "option" "own" "package"
                "record" "resource" "result" "s16" "s32" "s64" "s8"
                "static" "stream" "string" "tuple" "type" "u16" "u32"
                "u64" "u8" "use" "variant" "with" "world" "async")))
    (let loop ((ks kws))
      (if (not (null? ks))
          (begin
            (string-ht-set! *wit-keywords* (car ks) #t)
            (loop (cdr ks)))))))

(init-wit-keywords!)

(define (wit-keyword? s)
  (string-ht-has? *wit-keywords* s))

;;; --- Tokenizer ---

;; Lexer state: #(port line col peeked filename)
(define (make-wit-lexer port filename)
  (vector port 1 0 #f filename))

(define (lex-port lx) (vector-ref lx 0))
(define (lex-line lx) (vector-ref lx 1))
(define (lex-col lx) (vector-ref lx 2))
(define (lex-peeked lx) (vector-ref lx 3))
(define (lex-filename lx) (vector-ref lx 4))
(define (lex-set-line! lx v) (vector-set! lx 1 v))
(define (lex-set-col! lx v) (vector-set! lx 2 v))
(define (lex-set-peeked! lx v) (vector-set! lx 3 v))

(define (wit-error lx msg)
  (display (string-append (lex-filename lx) ":"
                          (number->string (lex-line lx)) ": error: "
                          msg "\n")
           (current-error-port))
  (exit 1))

(define (lex-read-char lx)
  (let ((c (read-char (lex-port lx))))
    (if (not (eof-object? c))
        (if (char=? c #\newline)
            (begin (lex-set-line! lx (+ (lex-line lx) 1))
                   (lex-set-col! lx 0))
            (lex-set-col! lx (+ (lex-col lx) 1))))
    c))

(define (lex-peek-char lx)
  (peek-char (lex-port lx)))

(define (wit-skip-whitespace lx)
  (let loop ()
    (let ((c (lex-peek-char lx)))
      (cond
        ((eof-object? c) #f)
        ((or (char=? c #\space) (char=? c #\tab)
             (char=? c #\newline) (char=? c #\return))
         (lex-read-char lx) (loop))
        ((char=? c #\/)
         (lex-read-char lx)
         (let ((c2 (lex-peek-char lx)))
           (cond
             ((and (not (eof-object? c2)) (char=? c2 #\/))
              ;; Line comment
              (lex-read-char lx)
              (let skip ()
                (let ((c3 (lex-peek-char lx)))
                  (if (or (eof-object? c3) (char=? c3 #\newline))
                      (begin (if (not (eof-object? c3)) (lex-read-char lx))
                             (loop))
                      (begin (lex-read-char lx) (skip))))))
             ((and (not (eof-object? c2)) (char=? c2 #\*))
              ;; Block comment (nestable)
              (lex-read-char lx)
              (let nest ((depth 1))
                (let ((c3 (lex-read-char lx)))
                  (cond
                    ((eof-object? c3)
                     (wit-error lx "unterminated block comment"))
                    ((char=? c3 #\/)
                     (if (and (not (eof-object? (lex-peek-char lx)))
                              (char=? (lex-peek-char lx) #\*))
                         (begin (lex-read-char lx) (nest (+ depth 1)))
                         (nest depth)))
                    ((char=? c3 #\*)
                     (if (and (not (eof-object? (lex-peek-char lx)))
                              (char=? (lex-peek-char lx) #\/))
                         (begin (lex-read-char lx)
                                (if (> depth 1)
                                    (nest (- depth 1))
                                    (loop)))
                         (nest depth)))
                    (else (nest depth))))))
             (else
              ;; Not a comment — it was just '/' (punct)
              ;; Push back by returning the '/' token directly
              ;; We handle this by returning #t to signal we found a '/'
              #t))))
        (else #f)))))

(define (wit-id-start? c)
  (or (and (char>=? c #\a) (char<=? c #\z))
      (and (char>=? c #\A) (char<=? c #\Z))))

(define (wit-id-char? c)
  (or (and (char>=? c #\a) (char<=? c #\z))
      (and (char>=? c #\A) (char<=? c #\Z))
      (and (char>=? c #\0) (char<=? c #\9))
      (char=? c #\-)))

(define (wit-digit? c)
  (and (char>=? c #\0) (char<=? c #\9)))

(define (wit-read-identifier lx first-char)
  (let loop ((chars (list first-char)))
    (let ((c (lex-peek-char lx)))
      (if (and (not (eof-object? c)) (wit-id-char? c))
          (begin (lex-read-char lx) (loop (cons c chars)))
          (list->string (reverse chars))))))

(define (wit-read-integer lx first-char)
  (let loop ((chars (list first-char)))
    (let ((c (lex-peek-char lx)))
      (if (and (not (eof-object? c)) (wit-digit? c))
          (begin (lex-read-char lx) (loop (cons c chars)))
          (string->number (list->string (reverse chars)))))))

(define (wit-next-token lx)
  (let ((peeked (lex-peeked lx)))
    (if peeked
        (begin (lex-set-peeked! lx #f) peeked)
        (wit-next-token* lx))))

(define (wit-next-token* lx)
  (let ((slash? (wit-skip-whitespace lx)))
    (if (eq? slash? #t)
        ;; The '/' was consumed by skip-whitespace but wasn't a comment
        (list 'punct #\/)
        (let ((c (lex-peek-char lx)))
          (cond
            ((eof-object? c) '(eof))
            ((char=? c #\%)
             ;; Escaped identifier
             (lex-read-char lx)
             (let ((c2 (lex-peek-char lx)))
               (if (and (not (eof-object? c2)) (wit-id-start? c2))
                   (begin (lex-read-char lx)
                          (list 'id (wit-read-identifier lx c2)))
                   (wit-error lx "expected identifier after %"))))
            ((wit-id-start? c)
             (lex-read-char lx)
             (let ((name (wit-read-identifier lx c)))
               (if (wit-keyword? name)
                   (list 'keyword name)
                   (list 'id name))))
            ((wit-digit? c)
             (lex-read-char lx)
             (list 'integer (wit-read-integer lx c)))
            ((char=? c #\-)
             (lex-read-char lx)
             (let ((c2 (lex-peek-char lx)))
               (if (and (not (eof-object? c2)) (char=? c2 #\>))
                   (begin (lex-read-char lx) '(arrow))
                   (wit-error lx "unexpected '-' (expected '->')"))))
            ((char=? c #\@)
             (lex-read-char lx)
             (list 'punct #\@))
            ;; Underscore (used as wildcard in result<_, E>)
            ((char=? c #\_)
             (lex-read-char lx)
             (list 'punct #\_))
            ;; Single-char punctuation
            ((or (char=? c #\=) (char=? c #\,) (char=? c #\:)
                 (char=? c #\;) (char=? c #\.) (char=? c #\()
                 (char=? c #\)) (char=? c #\{) (char=? c #\})
                 (char=? c #\<) (char=? c #\>) (char=? c #\*))
             (lex-read-char lx)
             (list 'punct c))
            (else
             (lex-read-char lx)
             (wit-error lx (string-append "unexpected character '"
                                          (string c) "'"))))))))

(define (wit-peek-token lx)
  (if (lex-peeked lx)
      (lex-peeked lx)
      (let ((tok (wit-next-token* lx)))
        (lex-set-peeked! lx tok)
        tok)))

;;; --- Parser helpers ---

(define (wit-token-is? tok tag)
  (and (pair? tok) (eq? (car tok) tag)))

(define (wit-token-value tok)
  (if (and (pair? tok) (pair? (cdr tok)))
      (cadr tok)
      #f))

(define (wit-expect-keyword lx kw)
  (let ((tok (wit-next-token lx)))
    (if (and (wit-token-is? tok 'keyword)
             (string=? (wit-token-value tok) kw))
        tok
        (wit-error lx (string-append "expected '" kw "', got "
                                     (wit-token->string tok))))))

(define (wit-expect-punct lx ch)
  (let ((tok (wit-next-token lx)))
    (if (and (wit-token-is? tok 'punct)
             (char=? (wit-token-value tok) ch))
        tok
        (wit-error lx (string-append "expected '" (string ch) "', got "
                                     (wit-token->string tok))))))

(define (wit-expect-id lx)
  (let ((tok (wit-next-token lx)))
    (if (wit-token-is? tok 'id)
        (wit-token-value tok)
        ;; Keywords can also be used as identifiers in some contexts
        ;; but require % prefix. For strictness, error here.
        (wit-error lx (string-append "expected identifier, got "
                                     (wit-token->string tok))))))

(define (wit-expect-id-or-keyword lx)
  ;; In WIT, identifiers and keywords share the same character space.
  ;; Some contexts accept either.
  (let ((tok (wit-next-token lx)))
    (cond
      ((wit-token-is? tok 'id) (wit-token-value tok))
      ((wit-token-is? tok 'keyword) (wit-token-value tok))
      (else (wit-error lx (string-append "expected identifier, got "
                                         (wit-token->string tok)))))))

(define (wit-token->string tok)
  (cond
    ((wit-token-is? tok 'id) (string-append "id '" (wit-token-value tok) "'"))
    ((wit-token-is? tok 'keyword) (string-append "'" (wit-token-value tok) "'"))
    ((wit-token-is? tok 'integer) (string-append (number->string (wit-token-value tok))))
    ((wit-token-is? tok 'punct)
     (string-append "'" (string (wit-token-value tok)) "'"))
    ((wit-token-is? tok 'arrow) "'->'")
    ((wit-token-is? tok 'eof) "end of file")
    (else "unknown token")))

(define (wit-check-punct? lx ch)
  (let ((tok (wit-peek-token lx)))
    (and (wit-token-is? tok 'punct) (char=? (wit-token-value tok) ch))))

(define (wit-check-keyword? lx kw)
  (let ((tok (wit-peek-token lx)))
    (and (wit-token-is? tok 'keyword) (string=? (wit-token-value tok) kw))))

(define (wit-check-eof? lx)
  (wit-token-is? (wit-peek-token lx) 'eof))

;;; --- Parse comma-separated list with optional trailing comma ---

(define (parse-comma-list lx close-char parse-item)
  ;; Parse items separated by commas, with optional trailing comma,
  ;; terminated by close-char. Consumes the close-char.
  (if (wit-check-punct? lx close-char)
      (begin (wit-next-token lx) '())
      (let loop ((acc '()))
        (let ((item (parse-item lx)))
          (if (wit-check-punct? lx #\,)
              (begin
                (wit-next-token lx)  ;; consume comma
                (if (wit-check-punct? lx close-char)
                    (begin (wit-next-token lx) (reverse (cons item acc)))
                    (loop (cons item acc))))
              (begin
                (wit-expect-punct lx close-char)
                (reverse (cons item acc))))))))

;;; --- Type parser ---

(define (parse-wit-type lx)
  (let ((tok (wit-peek-token lx)))
    (cond
      ;; Primitive types
      ((and (wit-token-is? tok 'keyword)
            (let ((kw (wit-token-value tok)))
              (or (string=? kw "u8") (string=? kw "u16")
                  (string=? kw "u32") (string=? kw "u64")
                  (string=? kw "s8") (string=? kw "s16")
                  (string=? kw "s32") (string=? kw "s64")
                  (string=? kw "f32") (string=? kw "f64")
                  (string=? kw "char") (string=? kw "bool")
                  (string=? kw "string"))))
       (wit-next-token lx)
       (list 'prim (wit-token-value tok)))

      ;; list<T>
      ((wit-check-keyword? lx "list")
       (wit-next-token lx)
       (wit-expect-punct lx #\<)
       (let ((ty (parse-wit-type lx)))
         (wit-expect-punct lx #\>)
         (list 'list ty)))

      ;; option<T>
      ((wit-check-keyword? lx "option")
       (wit-next-token lx)
       (wit-expect-punct lx #\<)
       (let ((ty (parse-wit-type lx)))
         (wit-expect-punct lx #\>)
         (list 'option ty)))

      ;; tuple<T, ...>
      ((wit-check-keyword? lx "tuple")
       (wit-next-token lx)
       (wit-expect-punct lx #\<)
       (let ((types (parse-comma-list lx #\> parse-wit-type)))
         (cons 'tuple types)))

      ;; result, result<T>, result<T, E>, result<_, E>
      ((wit-check-keyword? lx "result")
       (wit-next-token lx)
       (if (wit-check-punct? lx #\<)
           (begin
             (wit-next-token lx)
             (let ((ok (if (wit-check-punct? lx #\_)
                           (begin (wit-next-token lx) #f)
                           (parse-wit-type lx))))
               (if (wit-check-punct? lx #\,)
                   (begin
                     (wit-next-token lx)
                     (let ((err (parse-wit-type lx)))
                       (wit-expect-punct lx #\>)
                       (list 'result ok err)))
                   (begin
                     (wit-expect-punct lx #\>)
                     (list 'result ok #f)))))
           (list 'result #f #f)))

      ;; future, future<T>
      ((wit-check-keyword? lx "future")
       (wit-next-token lx)
       (if (wit-check-punct? lx #\<)
           (begin
             (wit-next-token lx)
             (let ((ty (parse-wit-type lx)))
               (wit-expect-punct lx #\>)
               (list 'future ty)))
           (list 'future #f)))

      ;; stream, stream<T>
      ((wit-check-keyword? lx "stream")
       (wit-next-token lx)
       (if (wit-check-punct? lx #\<)
           (begin
             (wit-next-token lx)
             (let ((ty (parse-wit-type lx)))
               (wit-expect-punct lx #\>)
               (list 'stream ty)))
           (list 'stream #f)))

      ;; borrow<T>
      ((wit-check-keyword? lx "borrow")
       (wit-next-token lx)
       (wit-expect-punct lx #\<)
       (let ((name (wit-expect-id-or-keyword lx)))
         (wit-expect-punct lx #\>)
         (list 'borrow name)))

      ;; own<T> (surface syntax uses bare name, but allow explicit own<T>)
      ((wit-check-keyword? lx "own")
       (wit-next-token lx)
       (wit-expect-punct lx #\<)
       (let ((name (wit-expect-id-or-keyword lx)))
         (wit-expect-punct lx #\>)
         (list 'own name)))

      ;; User-defined type reference
      ((wit-token-is? tok 'id)
       (wit-next-token lx)
       (list 'ref (wit-token-value tok)))

      (else
       (wit-error lx (string-append "expected type, got "
                                    (wit-token->string tok)))))))

;;; --- Function type parser ---

(define (parse-wit-param-list lx)
  ;; '(' named-type-list ')'
  (wit-expect-punct lx #\()
  (parse-comma-list lx #\) parse-wit-named-type))

(define (parse-wit-named-type lx)
  ;; id ':' ty
  (let ((name (wit-expect-id-or-keyword lx)))
    (wit-expect-punct lx #\:)
    (let ((ty (parse-wit-type lx)))
      (cons name ty))))

(define (parse-wit-result-list lx)
  ;; '->' ty | empty
  (let ((tok (wit-peek-token lx)))
    (if (wit-token-is? tok 'arrow)
        (begin (wit-next-token lx) (parse-wit-type lx))
        #f)))

(define (parse-wit-func-type lx)
  ;; 'func' param-list result-list
  (wit-expect-keyword lx "func")
  (let* ((params (parse-wit-param-list lx))
         (result (parse-wit-result-list lx)))
    (cons params result)))

;;; --- Gate parser ---

(define (parse-wit-gate lx)
  ;; Parse @since(...) @unstable(...) @deprecated(...) annotations
  (let loop ((gates '()))
    (if (wit-check-punct? lx #\@)
        (begin
          (wit-next-token lx) ;; consume @
          (let ((name (wit-expect-id-or-keyword lx)))
            (wit-expect-punct lx #\()
            (cond
              ((string=? name "since")
               (let ((vk (wit-expect-id-or-keyword lx)))
                 (if (not (string=? vk "version"))
                     (wit-error lx "expected 'version' in @since"))
                 (wit-expect-punct lx #\=)
                 (let ((ver (parse-wit-semver lx)))
                   (wit-expect-punct lx #\))
                   (loop (cons (list 'since ver) gates)))))
              ((string=? name "unstable")
               (let ((fk (wit-expect-id-or-keyword lx)))
                 (if (not (string=? fk "feature"))
                     (wit-error lx "expected 'feature' in @unstable"))
                 (wit-expect-punct lx #\=)
                 (let ((feat (wit-expect-id-or-keyword lx)))
                   (wit-expect-punct lx #\))
                   (loop (cons (list 'unstable feat) gates)))))
              ((string=? name "deprecated")
               (let ((vk (wit-expect-id-or-keyword lx)))
                 (if (not (string=? vk "version"))
                     (wit-error lx "expected 'version' in @deprecated"))
                 (wit-expect-punct lx #\=)
                 (let ((ver (parse-wit-semver lx)))
                   (wit-expect-punct lx #\))
                   (loop (cons (list 'deprecated ver) gates)))))
              (else
               (wit-error lx (string-append "unknown gate '@" name "'"))))))
        (if (null? gates) #f (reverse gates)))))

(define (parse-wit-semver lx)
  ;; Parse semver: major.minor.patch[-prerelease][+build]
  ;; Simplified: read digits.digits.digits then optional -/+ segments
  (let ((tok (wit-next-token lx)))
    (if (not (wit-token-is? tok 'integer))
        (wit-error lx "expected version number"))
    (let ((major (number->string (wit-token-value tok))))
      (wit-expect-punct lx #\.)
      (let ((tok2 (wit-next-token lx)))
        (if (not (wit-token-is? tok2 'integer))
            (wit-error lx "expected version number"))
        (let ((minor (number->string (wit-token-value tok2))))
          (wit-expect-punct lx #\.)
          (let ((tok3 (wit-next-token lx)))
            (if (not (wit-token-is? tok3 'integer))
                (wit-error lx "expected version number"))
            (let ((patch (number->string (wit-token-value tok3))))
              ;; Check for -prerelease or +build
              ;; For simplicity, just read the digits part
              (string-append major "." minor "." patch))))))))

;;; --- Item parsers ---

(define (parse-wit-func-item lx name gate)
  ;; name ':' func-type ';'
  ;; name already consumed, ':' already consumed
  (let* ((ftype (parse-wit-func-type lx))
         (params (car ftype))
         (result (cdr ftype)))
    (wit-expect-punct lx #\;)
    (make-wit-func name params result gate)))

(define (parse-wit-record-item lx gate)
  ;; 'record' id '{' record-fields '}'
  (let ((name (wit-expect-id-or-keyword lx)))
    (wit-expect-punct lx #\{)
    (let ((fields (parse-comma-list lx #\} parse-wit-named-type)))
      (make-wit-record name fields gate))))

(define (parse-wit-variant-item lx gate)
  ;; 'variant' id '{' variant-cases '}'
  (let ((name (wit-expect-id-or-keyword lx)))
    (wit-expect-punct lx #\{)
    (let ((cases (parse-comma-list lx #\} parse-wit-variant-case)))
      (make-wit-variant name cases gate))))

(define (parse-wit-variant-case lx)
  ;; id | id '(' ty ')'
  (let ((gate (parse-wit-gate lx)))
    (let ((name (wit-expect-id-or-keyword lx)))
      (if (wit-check-punct? lx #\()
          (begin
            (wit-next-token lx)
            (let ((ty (parse-wit-type lx)))
              (wit-expect-punct lx #\))
              (cons name ty)))
          (cons name #f)))))

(define (parse-wit-enum-item lx gate)
  ;; 'enum' id '{' enum-cases '}'
  (let ((name (wit-expect-id-or-keyword lx)))
    (wit-expect-punct lx #\{)
    (let ((cases (parse-comma-list lx #\} parse-wit-enum-case)))
      (make-wit-enum name cases gate))))

(define (parse-wit-enum-case lx)
  (let ((gate (parse-wit-gate lx)))
    (wit-expect-id-or-keyword lx)))

(define (parse-wit-flags-item lx gate)
  ;; 'flags' id '{' flags-fields '}'
  (let ((name (wit-expect-id-or-keyword lx)))
    (wit-expect-punct lx #\{)
    (let ((fields (parse-comma-list lx #\} parse-wit-flags-field)))
      (make-wit-flags name fields gate))))

(define (parse-wit-flags-field lx)
  (let ((gate (parse-wit-gate lx)))
    (wit-expect-id-or-keyword lx)))

(define (parse-wit-resource-item lx gate)
  ;; 'resource' id ';' | 'resource' id '{' resource-method* '}'
  (let ((name (wit-expect-id-or-keyword lx)))
    (cond
      ((wit-check-punct? lx #\;)
       (wit-next-token lx)
       (make-wit-resource name '() gate))
      ((wit-check-punct? lx #\{)
       (wit-next-token lx)
       (let loop ((methods '()))
         (if (wit-check-punct? lx #\})
             (begin (wit-next-token lx)
                    (make-wit-resource name (reverse methods) gate))
             (let ((mgate (parse-wit-gate lx)))
               (cond
                 ;; constructor(params);
                 ((wit-check-keyword? lx "constructor")
                  (wit-next-token lx)
                  (let ((params (parse-wit-param-list lx)))
                    (wit-expect-punct lx #\;)
                    (loop (cons (make-wit-func "constructor" params #f mgate)
                                methods))))
                 ;; id: static func-type ;
                 ;; id: func-type ;
                 (else
                  (let ((mname (wit-expect-id-or-keyword lx)))
                    (wit-expect-punct lx #\:)
                    (if (wit-check-keyword? lx "static")
                        (begin
                          (wit-next-token lx)
                          (let* ((ftype (parse-wit-func-type lx))
                                 (params (car ftype))
                                 (result (cdr ftype)))
                            (wit-expect-punct lx #\;)
                            (loop (cons (make-wit-func
                                         (string-append "[static]" mname)
                                         params result mgate)
                                        methods))))
                        (let* ((ftype (parse-wit-func-type lx))
                               (params (car ftype))
                               (result (cdr ftype)))
                          (wit-expect-punct lx #\;)
                          (loop (cons (make-wit-func mname params result mgate)
                                      methods)))))))))))
      (else
       (wit-error lx "expected ';' or '{' after resource name")))))

(define (parse-wit-type-alias lx gate)
  ;; 'type' id '=' ty ';'
  (let ((name (wit-expect-id-or-keyword lx)))
    (wit-expect-punct lx #\=)
    (let ((ty (parse-wit-type lx)))
      (wit-expect-punct lx #\;)
      (make-wit-type-alias name ty gate))))

;;; --- Use-path parser ---

(define (parse-wit-use-path lx)
  ;; id
  ;; id ':' id '/' id ('@' semver)?
  (let ((first (wit-expect-id-or-keyword lx)))
    (if (wit-check-punct? lx #\:)
        (begin
          (wit-next-token lx) ;; consume ':'
          (let ((pkg (wit-expect-id-or-keyword lx)))
            (wit-expect-punct lx #\/)
            (let ((iface (wit-expect-id-or-keyword lx)))
              (let ((ver (if (wit-check-punct? lx #\@)
                             (begin (wit-next-token lx)
                                    (parse-wit-semver lx))
                             #f)))
                (list 'package first pkg iface ver)))))
        (list 'local first))))

;;; --- Use item parser (inside interface/world) ---

(define (parse-wit-use-item lx gate)
  ;; 'use' use-path '.' '{' use-names-list '}' ';'
  (let ((path (parse-wit-use-path lx)))
    (wit-expect-punct lx #\.)
    (wit-expect-punct lx #\{)
    (let ((names (parse-comma-list lx #\} parse-wit-use-name)))
      (wit-expect-punct lx #\;)
      (make-wit-use path names gate))))

(define (parse-wit-use-name lx)
  ;; id | id 'as' id
  (let ((name (wit-expect-id-or-keyword lx)))
    (if (wit-check-keyword? lx "as")
        (begin (wit-next-token lx)
               (let ((alias (wit-expect-id-or-keyword lx)))
                 (cons name alias)))
        (cons name #f))))

;;; --- Interface parser ---

(define (parse-wit-interface lx gate)
  ;; 'interface' id '{' interface-items* '}'
  (let ((name (wit-expect-id-or-keyword lx)))
    (wit-expect-punct lx #\{)
    (let loop ((items '()))
      (if (wit-check-punct? lx #\})
          (begin (wit-next-token lx)
                 (make-wit-interface name (reverse items) gate))
          (let ((igate (parse-wit-gate lx)))
            (let ((tok (wit-peek-token lx)))
              (cond
                ;; use ...
                ((wit-check-keyword? lx "use")
                 (wit-next-token lx)
                 (loop (cons (parse-wit-use-item lx igate) items)))
                ;; record
                ((wit-check-keyword? lx "record")
                 (wit-next-token lx)
                 (loop (cons (parse-wit-record-item lx igate) items)))
                ;; variant
                ((wit-check-keyword? lx "variant")
                 (wit-next-token lx)
                 (loop (cons (parse-wit-variant-item lx igate) items)))
                ;; enum
                ((wit-check-keyword? lx "enum")
                 (wit-next-token lx)
                 (loop (cons (parse-wit-enum-item lx igate) items)))
                ;; flags
                ((wit-check-keyword? lx "flags")
                 (wit-next-token lx)
                 (loop (cons (parse-wit-flags-item lx igate) items)))
                ;; resource
                ((wit-check-keyword? lx "resource")
                 (wit-next-token lx)
                 (loop (cons (parse-wit-resource-item lx igate) items)))
                ;; type alias
                ((wit-check-keyword? lx "type")
                 (wit-next-token lx)
                 (loop (cons (parse-wit-type-alias lx igate) items)))
                ;; func item: id ':' func-type ';'
                ((or (wit-token-is? tok 'id) (wit-token-is? tok 'keyword))
                 (let ((fname (wit-expect-id-or-keyword lx)))
                   (wit-expect-punct lx #\:)
                   (loop (cons (parse-wit-func-item lx fname igate) items))))
                (else
                 (wit-error lx (string-append
                                "unexpected token in interface: "
                                (wit-token->string tok)))))))))))

;;; --- World parser ---

(define (parse-wit-world lx gate)
  ;; 'world' id '{' world-items* '}'
  (let ((name (wit-expect-id-or-keyword lx)))
    (wit-expect-punct lx #\{)
    (let loop ((items '()))
      (if (wit-check-punct? lx #\})
          (begin (wit-next-token lx)
                 (make-wit-world name (reverse items) gate))
          (let ((wgate (parse-wit-gate lx)))
            (cond
              ;; import
              ((wit-check-keyword? lx "import")
               (wit-next-token lx)
               (loop (cons (parse-wit-world-import lx wgate) items)))
              ;; export
              ((wit-check-keyword? lx "export")
               (wit-next-token lx)
               (loop (cons (parse-wit-world-export lx wgate) items)))
              ;; use
              ((wit-check-keyword? lx "use")
               (wit-next-token lx)
               (loop (cons (parse-wit-use-item lx wgate) items)))
              ;; include
              ((wit-check-keyword? lx "include")
               (wit-next-token lx)
               (loop (cons (parse-wit-include-item lx wgate) items)))
              ;; typedef items in world
              ((wit-check-keyword? lx "record")
               (wit-next-token lx)
               (loop (cons (parse-wit-record-item lx wgate) items)))
              ((wit-check-keyword? lx "variant")
               (wit-next-token lx)
               (loop (cons (parse-wit-variant-item lx wgate) items)))
              ((wit-check-keyword? lx "enum")
               (wit-next-token lx)
               (loop (cons (parse-wit-enum-item lx wgate) items)))
              ((wit-check-keyword? lx "flags")
               (wit-next-token lx)
               (loop (cons (parse-wit-flags-item lx wgate) items)))
              ((wit-check-keyword? lx "resource")
               (wit-next-token lx)
               (loop (cons (parse-wit-resource-item lx wgate) items)))
              ((wit-check-keyword? lx "type")
               (wit-next-token lx)
               (loop (cons (parse-wit-type-alias lx wgate) items)))
              (else
               (wit-error lx (string-append
                              "unexpected token in world: "
                              (wit-token->string (wit-peek-token lx)))))))))))

(define (parse-wit-world-import lx gate)
  ;; 'import' id ':' extern-type | 'import' use-path ';'
  (let ((tok (wit-peek-token lx)))
    ;; Peek ahead: if we see id ':' it's a named import
    ;; If we see id ':' id '/' or id ';' it could be a use-path
    ;; We need to disambiguate: read the name, check what follows
    (let ((name (wit-expect-id-or-keyword lx)))
      (cond
        ((wit-check-punct? lx #\:)
         ;; Could be named import (id ':' extern-type) or package path (ns ':' pkg)
         ;; Check if the next-next token looks like a func keyword or '{'
         (let ((tok2 (begin (wit-next-token lx) (wit-peek-token lx))))
           (cond
             ;; func-type
             ((or (wit-check-keyword? lx "func")
                  (wit-check-keyword? lx "async"))
              (let* ((ftype (parse-wit-func-type lx))
                     (params (car ftype))
                     (result (cdr ftype)))
                (wit-expect-punct lx #\;)
                (list 'import name (make-wit-func name params result gate))))
             ;; inline interface
             ((wit-check-keyword? lx "interface")
              (wit-next-token lx)
              (wit-expect-punct lx #\{)
              (let ((iface (parse-wit-inline-interface lx)))
                (list 'import-interface name iface)))
             ;; package path: we already consumed 'name' and ':', so this is ns:pkg/iface
             (else
              (let ((pkg (wit-expect-id-or-keyword lx)))
                (wit-expect-punct lx #\/)
                (let ((iface (wit-expect-id-or-keyword lx)))
                  (let ((ver (if (wit-check-punct? lx #\@)
                                 (begin (wit-next-token lx)
                                        (parse-wit-semver lx))
                                 #f)))
                    (wit-expect-punct lx #\;)
                    (list 'import-path
                          (list 'package name pkg iface ver)))))))))
        ;; No colon — must be local path: 'import' id ';'
        ((wit-check-punct? lx #\;)
         (wit-next-token lx)
         (list 'import-path (list 'local name)))
        (else
         (wit-error lx "expected ':' or ';' after import name"))))))

(define (parse-wit-world-export lx gate)
  ;; Same structure as import
  (let ((name (wit-expect-id-or-keyword lx)))
    (cond
      ((wit-check-punct? lx #\:)
       (let ((tok2 (begin (wit-next-token lx) (wit-peek-token lx))))
         (cond
           ((or (wit-check-keyword? lx "func")
                (wit-check-keyword? lx "async"))
            (let* ((ftype (parse-wit-func-type lx))
                   (params (car ftype))
                   (result (cdr ftype)))
              (wit-expect-punct lx #\;)
              (list 'export name (make-wit-func name params result gate))))
           ((wit-check-keyword? lx "interface")
            (wit-next-token lx)
            (wit-expect-punct lx #\{)
            (let ((iface (parse-wit-inline-interface lx)))
              (list 'export-interface name iface)))
           (else
            (let ((pkg (wit-expect-id-or-keyword lx)))
              (wit-expect-punct lx #\/)
              (let ((iface (wit-expect-id-or-keyword lx)))
                (let ((ver (if (wit-check-punct? lx #\@)
                               (begin (wit-next-token lx)
                                      (parse-wit-semver lx))
                               #f)))
                  (wit-expect-punct lx #\;)
                  (list 'export-path
                        (list 'package name pkg iface ver)))))))))
      ((wit-check-punct? lx #\;)
       (wit-next-token lx)
       (list 'export-path (list 'local name)))
      (else
       (wit-error lx "expected ':' or ';' after export name")))))

(define (parse-wit-inline-interface lx)
  ;; Parse interface body items until '}'
  (let loop ((items '()))
    (if (wit-check-punct? lx #\})
        (begin (wit-next-token lx) (reverse items))
        (let ((igate (parse-wit-gate lx)))
          (let ((tok (wit-peek-token lx)))
            (cond
              ((wit-check-keyword? lx "use")
               (wit-next-token lx)
               (loop (cons (parse-wit-use-item lx igate) items)))
              ((wit-check-keyword? lx "record")
               (wit-next-token lx)
               (loop (cons (parse-wit-record-item lx igate) items)))
              ((wit-check-keyword? lx "variant")
               (wit-next-token lx)
               (loop (cons (parse-wit-variant-item lx igate) items)))
              ((wit-check-keyword? lx "enum")
               (wit-next-token lx)
               (loop (cons (parse-wit-enum-item lx igate) items)))
              ((wit-check-keyword? lx "flags")
               (wit-next-token lx)
               (loop (cons (parse-wit-flags-item lx igate) items)))
              ((wit-check-keyword? lx "resource")
               (wit-next-token lx)
               (loop (cons (parse-wit-resource-item lx igate) items)))
              ((wit-check-keyword? lx "type")
               (wit-next-token lx)
               (loop (cons (parse-wit-type-alias lx igate) items)))
              ((or (wit-token-is? tok 'id) (wit-token-is? tok 'keyword))
               (let ((fname (wit-expect-id-or-keyword lx)))
                 (wit-expect-punct lx #\:)
                 (loop (cons (parse-wit-func-item lx fname igate) items))))
              (else
               (wit-error lx (string-append
                              "unexpected token in inline interface: "
                              (wit-token->string tok))))))))))

(define (parse-wit-include-item lx gate)
  ;; 'include' use-path ';' | 'include' use-path 'with' '{' include-names-list '}'
  (let ((path (parse-wit-use-path lx)))
    (cond
      ((wit-check-punct? lx #\;)
       (wit-next-token lx)
       (list 'include path '()))
      ((wit-check-keyword? lx "with")
       (wit-next-token lx)
       (wit-expect-punct lx #\{)
       (let ((renames (parse-comma-list lx #\} parse-wit-include-rename)))
         (list 'include path renames)))
      (else
       (wit-error lx "expected ';' or 'with' after include path")))))

(define (parse-wit-include-rename lx)
  ;; id 'as' id
  (let ((name (wit-expect-id-or-keyword lx)))
    (wit-expect-keyword lx "as")
    (let ((alias (wit-expect-id-or-keyword lx)))
      (cons name alias))))

;;; --- Package declaration parser ---

(define (parse-wit-package-decl lx)
  ;; 'package' ns ':' name ('@' semver)? ';'
  ;; Can also have nested namespaces: ns ':' name ('/' name)* ('@' semver)?
  (let ((ns (wit-expect-id-or-keyword lx)))
    (wit-expect-punct lx #\:)
    (let ((name (wit-expect-id-or-keyword lx)))
      (let ((ver (if (wit-check-punct? lx #\@)
                     (begin (wit-next-token lx) (parse-wit-semver lx))
                     #f)))
        (wit-expect-punct lx #\;)
        (list ns name ver)))))

;;; --- Top-level file parser ---

(define (parse-wit-file path)
  (let ((port (open-input-file path)))
    (let ((lx (make-wit-lexer port path)))
      (let ((result (parse-wit-file* lx)))
        (close-input-port port)
        result))))

(define (parse-wit-file* lx)
  ;; Parse a complete .wit file: optional package-decl, then interfaces and worlds
  (let ((pkg-decl
         (if (wit-check-keyword? lx "package")
             (begin (wit-next-token lx) (parse-wit-package-decl lx))
             #f))
        (ifaces '())
        (worlds '()))
    (let loop ((ifaces '()) (worlds '()) (top-uses '()))
      (if (wit-check-eof? lx)
          (let ((ns (if pkg-decl (car pkg-decl) ""))
                (name (if pkg-decl (cadr pkg-decl) ""))
                (ver (if pkg-decl (caddr pkg-decl) #f)))
            (make-wit-package ns name ver (reverse ifaces) (reverse worlds)))
          (let ((gate (parse-wit-gate lx)))
            (cond
              ;; Top-level use
              ((wit-check-keyword? lx "use")
               (wit-next-token lx)
               ;; top-level use: 'use' use-path ('as' id)? ';'
               (let ((path (parse-wit-use-path lx)))
                 (let ((alias (if (wit-check-keyword? lx "as")
                                  (begin (wit-next-token lx)
                                         (wit-expect-id-or-keyword lx))
                                  #f)))
                   (wit-expect-punct lx #\;)
                   (loop ifaces worlds
                         (cons (list 'top-use path alias) top-uses)))))
              ;; interface
              ((wit-check-keyword? lx "interface")
               (wit-next-token lx)
               (loop (cons (parse-wit-interface lx gate) ifaces) worlds top-uses))
              ;; world
              ((wit-check-keyword? lx "world")
               (wit-next-token lx)
               (loop ifaces (cons (parse-wit-world lx gate) worlds) top-uses))
              (else
               (wit-error lx (string-append
                              "expected 'interface', 'world', or 'use' at top level, got "
                              (wit-token->string (wit-peek-token lx)))))))))))

;;; --- Multi-file package parser ---

(define (parse-wit-package paths)
  ;; Parse multiple .wit files and merge into one package.
  ;; First file's package declaration wins.
  (if (null? paths)
      (make-wit-package "" "" #f '() '())
      (let ((first (parse-wit-file (car paths))))
        (let loop ((rest (cdr paths))
                   (ifaces (wit-package-interfaces first))
                   (worlds (wit-package-worlds first)))
          (if (null? rest)
              (make-wit-package
               (wit-package-namespace first)
               (wit-package-name first)
               (wit-package-version first)
               ifaces worlds)
              (let ((pkg (parse-wit-file (car rest))))
                (loop (cdr rest)
                      (append ifaces (wit-package-interfaces pkg))
                      (append worlds (wit-package-worlds pkg)))))))))
