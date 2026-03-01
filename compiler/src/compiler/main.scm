;;; main.scm — Puppy Scheme compiler entry point (Gambit)
;;; Replaces main.c: argument parsing, read loop, expand, analyze, codegen, output

(include "wasm.scm")
(include "macro.scm")
(include "expand.scm")
(include "analyze.scm")
(include "../runtime/io.scm")
(include "../runtime/core.scm")
(include "../runtime/strings.scm")
(include "../runtime/equality.scm")
(include "../runtime/display.scm")
(include "../runtime/write.scm")
(include "../runtime/numbers.scm")
(include "../runtime/math.scm")
(include "../runtime/system.scm")
(include "../runtime/file-io.scm")
(include "../runtime/file-exists.scm")
(include "../runtime/read.scm")
(include "../runtime/vectors.scm")
(include "library.scm")
(include "wit.scm")
(include "component.scm")
(include "codegen-expr.scm")
(include "codegen.scm")

(define puppy-version "0.0.1")

(define (usage)
  (display "Usage: puppyc [options] <input.scm> [-o <output.wasm>]\n" (current-error-port))
  (display "\n" (current-error-port))
  (display "Options:\n" (current-error-port))
  (display "  -h, --help            Show this help message\n" (current-error-port))
  (display "  -v, --version         Show version\n" (current-error-port))
  (display "  -o <file>             Output file (default: input with .wasm extension)\n" (current-error-port))
  (display "  --lib-path <dirs>     Colon-separated library search paths\n" (current-error-port))
  (display "  --wit <file>          WIT file for Component Model (may be repeated)\n" (current-error-port))
  (display "  --core                Output core WASM module (skip component wrapping)\n" (current-error-port))
  (display "  --profile             Print per-phase timing to stderr\n" (current-error-port)))

(define TARGET_WASI 0)

(define (derive-output-path input-path)
  ;; "foo.scm" → "foo.wasm", "bar" → "bar.wasm"
  (let ((len (string-length input-path)))
    (if (and (>= len 4)
             (string=? (substring input-path (- len 4) len) ".scm"))
        (string-append (substring input-path 0 (- len 4)) ".wasm")
        (string-append input-path ".wasm"))))

(define (split-colon-path s)
  ;; Split colon-separated path string into list of directories (with trailing /)
  (let ((len (string-length s)))
    (let loop ((i 0) (start 0) (acc '()))
      (cond
        ((>= i len)
         (reverse (if (> i start)
                      (let ((dir (substring s start i)))
                        (cons (if (and (> (string-length dir) 0)
                                       (not (char=? (string-ref dir (- (string-length dir) 1)) #\/)))
                                  (string-append dir "/")
                                  dir)
                              acc))
                      acc)))
        ((char=? (string-ref s i) #\:)
         (let ((dir (substring s start i)))
           (loop (+ i 1) (+ i 1)
                 (if (> (string-length dir) 0)
                     (cons (if (not (char=? (string-ref dir (- (string-length dir) 1)) #\/))
                               (string-append dir "/")
                               dir)
                           acc)
                     acc))))
        (else (loop (+ i 1) start acc))))))

(define (parse-args args)
  ;; Returns (values target input-path output-path profile? lib-path wit-files core?) or exits
  (let loop ((args args)
             (target TARGET_WASI)
             (input #f)
             (output #f)
             (profile? #f)
             (lib-path '())
             (wit-files '())
             (core? #f))
    (if (null? args)
        (if input
            (values target input (or output (derive-output-path input))
                    profile? lib-path (reverse wit-files) core?)
            (begin (usage) (exit 1)))
        (let ((arg (car args))
              (rest (cdr args)))
          (cond
            ((or (string=? arg "-h") (string=? arg "--help"))
             (usage)
             (exit 0))
            ((or (string=? arg "-v") (string=? arg "--version"))
             (display (string-append puppy-version "\n"))
             (exit 0))
            ((string=? arg "--profile")
             (loop rest target input output #t lib-path wit-files core?))
            ((string=? arg "-o")
             (if (null? rest)
                 (begin
                   (display "error: -o requires an argument\n"
                            (current-error-port))
                   (exit 1))
                 (loop (cdr rest) target input (car rest) profile? lib-path
                       wit-files core?)))
            ((string=? arg "--target")
             (if (null? rest)
                 (begin
                   (display "error: --target requires an argument\n"
                            (current-error-port))
                   (exit 1))
                 (let ((tgt (car rest)))
                   (cond
                     ((string=? tgt "wasi")
                      (loop (cdr rest) TARGET_WASI input output profile? lib-path
                            wit-files core?))
                     (else
                      (display (string-append "error: unknown target '" tgt "'\n")
                               (current-error-port))
                      (exit 1))))))
            ((string=? arg "--lib-path")
             (if (null? rest)
                 (begin
                   (display "error: --lib-path requires an argument\n"
                            (current-error-port))
                   (exit 1))
                 (loop (cdr rest) target input output profile?
                       (append lib-path (split-colon-path (car rest)))
                       wit-files core?)))
            ((string=? arg "--wit")
             (if (null? rest)
                 (begin
                   (display "error: --wit requires an argument\n"
                            (current-error-port))
                   (exit 1))
                 (loop (cdr rest) target input output profile? lib-path
                       (cons (car rest) wit-files) core?)))
            ((string=? arg "--core")
             (loop rest target input output profile? lib-path wit-files #t))
            ;; Skip bare "--" (wasmtime passes it as a module argument)
            ((string=? arg "--")
             (loop rest target input output profile? lib-path wit-files core?))
            ;; Positional arg = input file
            ((not input)
             (loop rest target arg output profile? lib-path wit-files core?))
            (else
             (loop rest target input output profile? lib-path wit-files core?)))))))

(define (read-all-forms port)
  ;; Read all s-expressions from port, return as list
  (let loop ((acc '()))
    (let ((form (read port)))
      (if (eof-object? form)
          (reverse acc)
          (loop (cons form acc))))))

;;; --- Path utilities (pure Scheme replacements for Gambit builtins) ---

(define (path-directory path)
  ;; Return directory portion of path, with trailing /
  (let loop ((i (- (string-length path) 1)))
    (cond
      ((< i 0) "./")
      ((char=? (string-ref path i) #\/)
       (substring path 0 (+ i 1)))
      (else (loop (- i 1))))))

(define (path-expand filename base-dir)
  ;; If filename is absolute, return it; otherwise join base-dir/filename
  (if (and (> (string-length filename) 0)
           (char=? (string-ref filename 0) #\/))
      filename
      (string-append base-dir filename)))

(define (path-normalize path)
  ;; Split on /, resolve "." and "..", rejoin
  (let* ((len (string-length path))
         (absolute? (and (> len 0) (char=? (string-ref path 0) #\/)))
         ;; Split path into components
         (parts
          (let loop ((i 0) (start 0) (acc '()))
            (cond
              ((>= i len)
               (reverse (if (> i start)
                            (cons (substring path start i) acc)
                            acc)))
              ((char=? (string-ref path i) #\/)
               (loop (+ i 1) (+ i 1)
                     (if (> i start)
                         (cons (substring path start i) acc)
                         acc)))
              (else (loop (+ i 1) start acc)))))
         ;; Resolve . and ..
         (resolved
          (let loop ((ps parts) (stack '()))
            (if (null? ps)
                (reverse stack)
                (let ((p (car ps)))
                  (cond
                    ((string=? p ".") (loop (cdr ps) stack))
                    ((string=? p "..")
                     (loop (cdr ps) (if (null? stack) '() (cdr stack))))
                    (else (loop (cdr ps) (cons p stack))))))))
         ;; Rejoin
         (joined
          (if (null? resolved)
              (if absolute? "/" ".")
              (let loop ((ps resolved) (acc ""))
                (if (null? ps)
                    acc
                    (loop (cdr ps)
                          (string-append acc (if (string=? acc "") "" "/")
                                         (car ps))))))))
    (if absolute?
        (string-append "/" joined)
        joined)))

(define (resolve-include-path base-dir filename)
  (path-expand filename base-dir))

(define (process-includes forms base-dir visited)
  (let loop ((fs forms) (acc '()))
    (if (null? fs)
        (reverse acc)
        (let ((form (car fs)))
          (if (and (pair? form)
                   (symbol? (car form))
                   (string=? (symbol->string (car form)) "include")
                   (pair? (cdr form))
                   (string? (cadr form))
                   (null? (cddr form)))
              ;; It's (include "filename") — splice in the file's forms
              (let* ((filename (cadr form))
                     (full-path (resolve-include-path base-dir filename))
                     (norm-path (path-normalize full-path)))
                (if (string-member norm-path visited)
                    (begin
                      (display (string-append "error: circular include: '"
                                              filename "'\n")
                               (current-error-port))
                      (exit 1)))
                (let* ((included-forms
                        (call-with-input-file full-path read-all-forms))
                       (inc-dir (path-directory full-path))
                       (processed (process-includes included-forms inc-dir
                                                    (cons norm-path visited))))
                  ;; Fold processed onto acc (avoids intermediate reverse + append)
                  (let fold ((ps processed) (a acc))
                    (if (null? ps)
                        (loop (cdr fs) a)
                        (fold (cdr ps) (cons (car ps) a))))))
              ;; Not an include form — keep as-is
              (loop (cdr fs) (cons form acc)))))))

(define (write-wasm-file path bytes)
  (let ((port (open-output-file path)))
    (write-bytevector bytes port 0 (bytevector-length bytes))
    (close-output-port port)))

(define *profile* #f)

(define (phase-time label t0)
  (let ((elapsed (- (current-milliseconds) t0)))
    (display (string-append "  " label ": ") (current-error-port))
    (display elapsed (current-error-port))
    (display "ms\n" (current-error-port))))

(define (main)
  (let ((args (cdr (command-line))))
    (if (null? args)
        (begin (usage) (exit 1))
        (receive (target input-path output-path profile? lib-path wit-files core?)
                 (parse-args args)

          (set! *profile* profile?)

          ;; Parse WIT files if specified
          (when (not (null? wit-files))
            (let ((t0 (if profile? (current-milliseconds) 0)))
              (let ((wit-pkg (parse-wit-package wit-files)))
                (when profile? (phase-time "wit" t0))
                (if (null? (wit-package-worlds wit-pkg))
                    (begin
                      (display "error: no world found in WIT files\n" (current-error-port))
                      (exit 1))
                    (set! *wit-world* (car (wit-package-worlds wit-pkg)))))))

          ;; Read + include
          (let* ((t0 (if profile? (current-milliseconds) 0))
                 (raw-forms
                  (call-with-input-file input-path read-all-forms))
                 (base-dir (path-directory (path-normalize input-path)))
                 (forms (process-includes raw-forms base-dir
                                          (list (path-normalize input-path))))
                 (_ (when profile? (phase-time "read" t0))))

            (if (null? forms)
                (begin
                  (display "error: no expressions in input\n" (current-error-port))
                  (exit 1)))

            ;; Library resolution
            (let* ((t0 (if profile? (current-milliseconds) 0))
                   (search-paths (append (list base-dir
                                               (string-append base-dir "lib/"))
                                         lib-path))
                   (forms (resolve-libraries forms base-dir search-paths))
                   (_ (when profile? (phase-time "libraries" t0))))

            ;; Expand
            (let* ((t0 (if profile? (current-milliseconds) 0))
                   (expanded (expand-forms forms))
                   (_ (when profile? (phase-time "expand" t0))))

              (if (null? expanded)
                  (begin
                    (display "error: no expressions in input\n" (current-error-port))
                    (exit 1)))

              ;; Alpha-convert
              (let* ((t0 (if profile? (current-milliseconds) 0))
                     (expanded (alpha-convert-forms expanded))
                     (_ (when profile? (phase-time "alpha-convert" t0))))

              ;; Analyze
              (let* ((t0 (if profile? (current-milliseconds) 0))
                     (analysis (analyze-forms expanded target))
                     (_ (when profile? (phase-time "analyze" t0))))

                ;; Codegen
                (let* ((t0 (if profile? (current-milliseconds) 0))
                       (wasm-bytes (codegen-module expanded target analysis))
                       (_ (when profile? (phase-time "codegen" t0))))
                  (if (not wasm-bytes)
                      (begin
                        (display "error: code generation failed\n" (current-error-port))
                        (exit 1)))

                  ;; Wrap in component (unless --core)
                  (let ((final-bytes
                         (if core?
                            wasm-bytes
                            (if *wit-world*
                               (wrap-component wasm-bytes *wit-world*)
                               (wrap-wasi-component wasm-bytes
                                 (vector-ref (ar-index-map analysis) IDX-NEEDS-IO)
                                 (vector-ref (ar-index-map analysis) IDX-NEEDS-EXIT)
                                 (vector-ref (ar-flags analysis) FLAG-CLOCK)
                                 (vector-ref (ar-flags analysis) FLAG-COMMAND-LINE)
                                 (vector-ref (ar-flags analysis) FLAG-GET-ENV)
                                 (vector-ref (ar-flags analysis) FLAG-FILE-IO))))))

                    ;; Write .wasm output
                    (let ((t0 (if profile? (current-milliseconds) 0)))
                      (write-wasm-file output-path final-bytes)
                      (when profile? (phase-time "write" t0))))))))))))))

(main)
