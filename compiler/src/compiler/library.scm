;;; library.scm — R7RS define-library resolution
;;; Resolves library dependencies, renames internal definitions for namespace
;;; isolation, and flattens everything into a single form list.
;;; Inserted between read/include and expand phases.

;;; --- Library name utilities ---

(define (library-name->string name)
  ;; (puppy utils) → "puppy.utils"
  (let loop ((parts name) (acc ""))
    (if (null? parts)
        acc
        (loop (cdr parts)
              (string-append acc
                             (if (string=? acc "") "" ".")
                             (if (symbol? (car parts))
                                 (symbol->string (car parts))
                                 (number->string (car parts))))))))

(define (library-name->path name)
  ;; (puppy utils) → "puppy/utils.sld"
  (let loop ((parts name) (acc ""))
    (if (null? parts)
        (string-append acc ".sld")
        (loop (cdr parts)
              (string-append acc
                             (if (string=? acc "") "" "/")
                             (if (symbol? (car parts))
                                 (symbol->string (car parts))
                                 (number->string (car parts))))))))

(define (library-name=? a b)
  (cond
    ((and (null? a) (null? b)) #t)
    ((or (null? a) (null? b)) #f)
    ((not (equal? (car a) (car b))) #f)
    (else (library-name=? (cdr a) (cdr b)))))

;;; --- Standard library detection ---

(define (standard-library? name)
  ;; (scheme base), (scheme write), etc. are built-in — treated as no-ops
  (and (pair? name)
       (symbol? (car name))
       (string=? (symbol->string (car name)) "scheme")))

;;; --- Parse define-library form ---

(define (parse-library form)
  ;; Returns: (name imports exports begin-forms include-files)
  ;; form is (define-library <name> <decl> ...)
  (let ((name (cadr form))
        (decls (cddr form)))
    (let loop ((ds decls)
               (imports '())
               (exports '())
               (begins '())
               (includes '()))
      (if (null? ds)
          (list name (reverse imports) (reverse exports)
                (reverse begins) (reverse includes))
          (let ((d (car ds)))
            (if (not (pair? d))
                (loop (cdr ds) imports exports begins includes)
                (let ((key (symbol->string (car d))))
                  (cond
                    ((string=? key "import")
                     (loop (cdr ds)
                           (append (reverse (cdr d)) imports)
                           exports begins includes))
                    ((string=? key "export")
                     (loop (cdr ds) imports
                           (append (reverse (cdr d)) exports)
                           begins includes))
                    ((string=? key "begin")
                     (loop (cdr ds) imports exports
                           (append (reverse (cdr d)) begins)
                           includes))
                    ((string=? key "include")
                     (loop (cdr ds) imports exports begins
                           (append (reverse (cdr d)) includes)))
                    (else
                     (loop (cdr ds) imports exports begins includes))))))))))

;;; --- Find library file ---

(define (find-library-file name search-paths)
  ;; Try each search path for the .sld file, return first match or #f
  (let ((rel-path (library-name->path name)))
    (let loop ((paths search-paths))
      (if (null? paths)
          #f
          (let ((candidate (string-append (car paths) rel-path)))
            (if (file-exists? candidate)
                candidate
                (loop (cdr paths))))))))

;;; --- Collect defined names from forms ---

(define (collect-defined-names forms)
  ;; Extract all define/define-syntax names from body forms
  (let loop ((fs forms) (acc '()))
    (if (null? fs)
        (reverse acc)
        (let ((f (car fs)))
          (if (and (pair? f) (symbol? (car f)))
              (let ((head (symbol->string (car f))))
                (cond
                  ;; (define (name ...) body)
                  ((and (string=? head "define")
                        (pair? (cdr f))
                        (pair? (cadr f)))
                   (loop (cdr fs) (cons (caadr f) acc)))
                  ;; (define name expr)
                  ((and (string=? head "define")
                        (pair? (cdr f))
                        (symbol? (cadr f)))
                   (loop (cdr fs) (cons (cadr f) acc)))
                  ;; (define-syntax name ...)
                  ((and (string=? head "define-syntax")
                        (pair? (cdr f))
                        (symbol? (cadr f)))
                   (loop (cdr fs) (cons (cadr f) acc)))
                  (else (loop (cdr fs) acc))))
              (loop (cdr fs) acc))))))

;;; --- Rename library forms for namespace isolation ---

(define (make-prefixed-symbol prefix name)
  ;; prefix: "puppy.utils", name: helper → puppy.utils~helper
  (string->symbol (string-append prefix "~" (symbol->string name))))

(define (build-rename-alist prefix defined-names export-names)
  ;; Build alist: ((internal-name . prefixed-name) ...)
  ;; Only internal (non-exported) names get prefixed.
  (let loop ((names defined-names) (acc '()))
    (if (null? names)
        (reverse acc)
        (let ((name (car names)))
          (if (memq name export-names)
              (loop (cdr names) acc)
              (loop (cdr names)
                    (cons (cons name (make-prefixed-symbol prefix name))
                          acc)))))))

(define (rename-library-form form renames)
  ;; Scope-aware rename walk for library namespace isolation.
  ;; Similar to rename-free in expand.scm but handles define heads.
  (if (null? renames)
      form
      (cond
        ((symbol? form)
         (let ((entry (assq form renames)))
           (if entry (cdr entry) form)))

        ((not (pair? form)) form)
        ((null? form) form)

        ((not (symbol? (car form)))
         (map (lambda (x) (rename-library-form x renames)) form))

        (else
         (let ((head-str (symbol->string (car form))))
           (cond
             ;; quote — never rename inside
             ((string=? head-str "quote") form)

             ;; lambda — shadow params
             ((and (string=? head-str "lambda")
                   (>= (length form) 3)
                   (pair? (cadr form)))
              (let ((filtered (filter-renames renames (cadr form))))
                (cons 'lambda
                      (cons (cadr form)
                            (map (lambda (x) (rename-library-form x filtered))
                                 (cddr form))))))

             ;; (define (name args...) body...) — rename name, shadow args
             ((and (string=? head-str "define")
                   (pair? (cdr form))
                   (pair? (cadr form)))
              (let* ((sig (cadr form))
                     (name (car sig))
                     (params (cdr sig))
                     (renamed-name (let ((e (assq name renames)))
                                     (if e (cdr e) name)))
                     (filtered (filter-renames renames params)))
                (cons 'define
                      (cons (cons renamed-name params)
                            (map (lambda (x) (rename-library-form x filtered))
                                 (cddr form))))))

             ;; (define name expr) — rename name
             ((and (string=? head-str "define")
                   (pair? (cdr form))
                   (symbol? (cadr form)))
              (let ((renamed-name (let ((e (assq (cadr form) renames)))
                                    (if e (cdr e) (cadr form)))))
                (list 'define renamed-name
                      (rename-library-form (caddr form) renames))))

             ;; (define-syntax name ...) — rename name
             ((and (string=? head-str "define-syntax")
                   (pair? (cdr form))
                   (symbol? (cadr form)))
              (let ((renamed-name (let ((e (assq (cadr form) renames)))
                                    (if e (cdr e) (cadr form)))))
                (cons 'define-syntax
                      (cons renamed-name
                            (map (lambda (x) (rename-library-form x renames))
                                 (cddr form))))))

             ;; let — shadow binding names
             ((and (string=? head-str "let")
                   (>= (length form) 3)
                   (pair? (cadr form))
                   (or (null? (cadr form))
                       (pair? (caadr form))))
              (let* ((bindings (cadr form))
                     (new-bindings
                      (map (lambda (b)
                             (if (and (pair? b) (= (length b) 2))
                                 (list (car b)
                                       (rename-library-form (cadr b) renames))
                                 b))
                           bindings))
                     (bind-names (filter symbol?
                                        (map (lambda (b)
                                               (if (and (pair? b) (>= (length b) 1)
                                                        (symbol? (car b)))
                                                   (car b) '()))
                                             bindings)))
                     (filtered (filter-renames renames bind-names)))
                (cons 'let
                      (cons new-bindings
                            (map (lambda (x) (rename-library-form x filtered))
                                 (cddr form))))))

             ;; let* — progressive shadowing
             ((and (string=? head-str "let*")
                   (>= (length form) 3)
                   (pair? (cadr form)))
              (let* ((bindings (cadr form))
                     (result
                      (let loop ((bs bindings) (shadow '()) (acc '()))
                        (if (null? bs)
                            (cons (reverse acc) shadow)
                            (let* ((b (car bs))
                                   (filtered (filter-renames renames shadow))
                                   (new-b (if (and (pair? b) (= (length b) 2))
                                              (list (car b)
                                                    (rename-library-form (cadr b) filtered))
                                              b))
                                   (bname (if (and (pair? b) (symbol? (car b)))
                                              (car b) #f)))
                              (loop (cdr bs)
                                    (if bname (cons bname shadow) shadow)
                                    (cons new-b acc))))))
                     (new-bindings (car result))
                     (all-shadow (cdr result))
                     (filtered (filter-renames renames all-shadow)))
                (cons 'let*
                      (cons new-bindings
                            (map (lambda (x) (rename-library-form x filtered))
                                 (cddr form))))))

             ;; Default — recurse into all subforms
             (else
              (map (lambda (x) (rename-library-form x renames)) form))))))))

;;; --- Topological sort ---

(define (find-lib name libs)
  (let loop ((ls libs))
    (cond
      ((null? ls) #f)
      ((library-name=? name (car (car ls))) (car ls))
      (else (loop (cdr ls))))))

(define (toposort-libraries libs)
  ;; libs: list of (name imports exports begin-forms include-files)
  ;; Returns sorted list in dependency order, or errors on cycle.
  ;; Filters out standard library imports.
  (let ((visited (make-string-ht))
        (in-stack (make-string-ht))
        (result '()))

    (define (visit lib-name)
      (let ((key (library-name->string lib-name)))
        (when (string-ht-ref in-stack key #f)
          (display (string-append "error: circular library dependency: "
                                  key "\n")
                   (current-error-port))
          (exit 1))
        (unless (string-ht-has? visited key)
          (string-ht-set! in-stack key #t)
          (let ((lib (find-lib lib-name libs)))
            (when lib
              (for-each
               (lambda (imp)
                 (unless (standard-library? imp)
                   (visit imp)))
               (cadr lib))))
          (string-ht-set! visited key #t)
          (string-ht-set! in-stack key #f)
          (let ((lib (find-lib lib-name libs)))
            (when lib
              (set! result (cons lib result)))))))

    (for-each (lambda (lib) (visit (car lib))) libs)
    (reverse result)))

;;; --- Main resolver ---

(define (resolve-libraries forms base-dir search-paths)
  ;; Separate define-library, top-level import, and other forms.
  ;; If no libraries/imports, return forms unchanged (fast path).
  ;; Otherwise resolve, rename, and flatten.
  (let loop ((fs forms)
             (lib-forms '())
             (import-forms '())
             (other-forms '()))
    (if (null? fs)
        ;; Process collected forms
        (let ((lib-forms (reverse lib-forms))
              (import-forms (reverse import-forms))
              (other-forms (reverse other-forms)))
          (if (and (null? lib-forms) (null? import-forms))
              ;; Fast path: no libraries at all
              other-forms
              ;; Resolve libraries
              (resolve-libraries* lib-forms import-forms other-forms
                                  base-dir search-paths)))
        ;; Classify each form
        (let ((f (car fs)))
          (if (and (pair? f) (symbol? (car f)))
              (let ((head (symbol->string (car f))))
                (cond
                  ((string=? head "define-library")
                   (loop (cdr fs) (cons f lib-forms) import-forms other-forms))
                  ((string=? head "import")
                   (loop (cdr fs) lib-forms (cons f import-forms) other-forms))
                  (else
                   (loop (cdr fs) lib-forms import-forms (cons f other-forms)))))
              (loop (cdr fs) lib-forms import-forms (cons f other-forms)))))))

(define (resolve-libraries* lib-forms import-forms other-forms
                             base-dir search-paths)
  ;; 1. Parse all inline define-library forms
  (let ((parsed-libs
         (map (lambda (f) (parse-library f)) lib-forms)))

    ;; 2. Collect all imported library names (from top-level import forms)
    (let ((imported-names '()))
      (for-each
       (lambda (imp-form)
         ;; (import (lib name) (lib name2) ...)
         (for-each
          (lambda (lib-name)
            (unless (standard-library? lib-name)
              (set! imported-names (cons lib-name imported-names))))
          (cdr imp-form)))
       import-forms)

      ;; Also collect imports from inline libraries
      (for-each
       (lambda (plib)
         (for-each
          (lambda (imp)
            (unless (standard-library? imp)
              (unless (find-parsed-lib imp parsed-libs)
                (set! imported-names (cons imp imported-names)))))
          (cadr plib)))
       parsed-libs)

      ;; 3. Resolve external library files
      (let ((external-libs (resolve-external-libs
                            (dedupe-lib-names imported-names)
                            parsed-libs search-paths)))

        ;; 4. Combine all parsed libraries
        (let ((all-libs (append external-libs parsed-libs)))

          ;; 5. Handle include declarations inside libraries
          (let ((all-libs (map (lambda (plib)
                                 (process-library-includes plib base-dir search-paths))
                               all-libs)))

            ;; 6. Topological sort
            (let ((sorted (toposort-libraries all-libs)))

              ;; 7. Rename and collect forms
              (let ((renamed-forms '())
                    (exported-ht (make-string-ht)))

                ;; Process each library in order
                (for-each
                 (lambda (plib)
                   (let* ((name (car plib))
                          (imports (cadr plib))
                          (exports (caddr plib))
                          (begin-forms (cadddr plib))
                          (prefix (library-name->string name))
                          (defined (collect-defined-names begin-forms))
                          (renames (build-rename-alist prefix defined exports)))

                     ;; Check for export conflicts
                     (for-each
                      (lambda (exp-name)
                        (let ((s (symbol->string exp-name)))
                          (when (string-ht-has? exported-ht s)
                            (display (string-append
                                      "error: export conflict: '"
                                      s "' exported by multiple libraries\n")
                                     (current-error-port))
                            (exit 1))
                          (string-ht-set! exported-ht s #t)))
                      exports)

                     ;; Rename forms and accumulate
                     (for-each
                      (lambda (f)
                        (set! renamed-forms
                              (cons (rename-library-form f renames)
                                    renamed-forms)))
                      begin-forms)))
                 sorted)

                ;; Return: renamed library forms ++ user forms
                (append (reverse renamed-forms) other-forms)))))))))

(define (find-parsed-lib name parsed-libs)
  (let loop ((ls parsed-libs))
    (cond
      ((null? ls) #f)
      ((library-name=? name (car (car ls))) (car ls))
      (else (loop (cdr ls))))))

(define (dedupe-lib-names names)
  (let ((seen (make-string-ht)))
    (let loop ((ns names) (acc '()))
      (if (null? ns)
          (reverse acc)
          (let ((key (library-name->string (car ns))))
            (if (string-ht-has? seen key)
                (loop (cdr ns) acc)
                (begin
                  (string-ht-set! seen key #t)
                  (loop (cdr ns) (cons (car ns) acc)))))))))

(define (resolve-external-libs names parsed-libs search-paths)
  ;; For each name, if not already in parsed-libs, find and parse its .sld file.
  ;; Recursively resolve transitive deps.
  (let ((resolved (make-string-ht))
        (result '()))

    (define (resolve-one name)
      (let ((key (library-name->string name)))
        (unless (string-ht-has? resolved key)
          (string-ht-set! resolved key #t)
          (let ((path (find-library-file name search-paths)))
            (unless path
              (display (string-append "error: library not found: ("
                                      key ")\n")
                       (current-error-port))
              (exit 1))
            ;; Read and parse the .sld file
            (let* ((file-forms (call-with-input-file path read-all-forms))
                   (lib-form (find-define-library file-forms)))
              (unless lib-form
                (display (string-append "error: no define-library in "
                                        path "\n")
                         (current-error-port))
                (exit 1))
              (let ((plib (parse-library lib-form)))
                ;; Process includes relative to the library file
                (let ((plib (process-library-includes
                             plib (path-directory path) search-paths)))
                  (set! result (cons plib result))
                  ;; Recursively resolve this lib's imports
                  (for-each
                   (lambda (imp)
                     (unless (standard-library? imp)
                       (resolve-one imp)))
                   (cadr plib)))))))))

    ;; Mark already-parsed libs as resolved
    (for-each
     (lambda (plib)
       (string-ht-set! resolved (library-name->string (car plib)) #t))
     parsed-libs)

    (for-each resolve-one names)
    (reverse result)))

(define (find-define-library forms)
  ;; Find the first define-library form in a list
  (let loop ((fs forms))
    (cond
      ((null? fs) #f)
      ((and (pair? (car fs))
            (symbol? (caar fs))
            (string=? (symbol->string (caar fs)) "define-library"))
       (car fs))
      (else (loop (cdr fs))))))

(define (process-library-includes plib base-dir search-paths)
  ;; Handle (include ...) declarations inside a library.
  ;; Reads included files and appends their forms to begin-forms.
  (let* ((name (car plib))
         (imports (cadr plib))
         (exports (caddr plib))
         (begin-forms (cadddr plib))
         (include-files (car (cddddr plib))))
    (if (null? include-files)
        plib
        (let ((extra-forms
               (let loop ((files include-files) (acc '()))
                 (if (null? files)
                     (reverse acc)
                     (let* ((filename (car files))
                            (full-path (path-expand filename base-dir))
                            (included (call-with-input-file full-path
                                        read-all-forms)))
                       (loop (cdr files)
                             (append (reverse included) acc)))))))
          (list name imports exports
                (append begin-forms extra-forms) '())))))
