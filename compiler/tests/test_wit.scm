;;; test_wit.scm — Unit tests for the WIT parser
;;; Run: cd compiler && gsc -exe -o /tmp/test_wit tests/test_wit.scm && /tmp/test_wit

(include "../src/compiler/analyze.scm")
(include "../src/compiler/wit.scm")

;;; --- Test harness ---

(define *pass* 0)
(define *fail* 0)

(define (check name expected actual)
  (if (equal? expected actual)
      (set! *pass* (+ *pass* 1))
      (begin
        (set! *fail* (+ *fail* 1))
        (display "FAIL: ")
        (display name)
        (newline)
        (display "  expected: ")
        (write expected)
        (newline)
        (display "  actual:   ")
        (write actual)
        (newline))))

(define (report)
  (newline)
  (display *pass*)
  (display " passed, ")
  (display *fail*)
  (display " failed")
  (newline)
  (if (> *fail* 0) (exit 1)))

;;; --- Tokenizer tests ---

(define (make-string-lexer str)
  (make-wit-lexer (open-input-string str) "<test>"))

(define (tokens-from str)
  (let ((lx (make-string-lexer str)))
    (let loop ((acc '()))
      (let ((tok (wit-next-token lx)))
        (if (equal? tok '(eof))
            (reverse acc)
            (loop (cons tok acc)))))))

(check "tok: empty string"
       '()
       (tokens-from ""))

(check "tok: identifier"
       '((id "foo"))
       (tokens-from "foo"))

(check "tok: kebab-case id"
       '((id "my-thing"))
       (tokens-from "my-thing"))

(check "tok: keyword"
       '((keyword "func"))
       (tokens-from "func"))

(check "tok: escaped keyword"
       '((id "func"))
       (tokens-from "%func"))

(check "tok: integer"
       '((integer 42))
       (tokens-from "42"))

(check "tok: arrow"
       '((arrow))
       (tokens-from "->"))

(check "tok: punctuation"
       '((punct #\{) (punct #\}) (punct #\() (punct #\))
         (punct #\<) (punct #\>) (punct #\;) (punct #\,)
         (punct #\:) (punct #\=) (punct #\.) (punct #\*))
       (tokens-from "{}()<>;,:=.*"))

(check "tok: at sign"
       '((punct #\@) (id "since"))
       (tokens-from "@since"))

(check "tok: line comment"
       '((id "before") (id "after"))
       (tokens-from "before // comment\nafter"))

(check "tok: block comment"
       '((id "before") (id "after"))
       (tokens-from "before /* comment */ after"))

(check "tok: nested block comment"
       '((id "a") (id "b"))
       (tokens-from "a /* outer /* inner */ still comment */ b"))

(check "tok: slash punct"
       '((id "a") (punct #\/) (id "b"))
       (tokens-from "a / b"))

(check "tok: mixed tokens"
       '((keyword "func") (punct #\() (id "name") (punct #\:)
         (keyword "string") (punct #\)) (arrow) (keyword "bool"))
       (tokens-from "func(name: string) -> bool"))

(check "tok: all keywords are keywords"
       '((keyword "package") (keyword "world") (keyword "interface")
         (keyword "import") (keyword "export") (keyword "use")
         (keyword "type") (keyword "record") (keyword "variant")
         (keyword "enum") (keyword "flags") (keyword "resource")
         (keyword "func") (keyword "bool") (keyword "string")
         (keyword "u8") (keyword "u32") (keyword "s64") (keyword "f64")
         (keyword "list") (keyword "option") (keyword "result")
         (keyword "tuple") (keyword "borrow") (keyword "own"))
       (tokens-from "package world interface import export use type record variant enum flags resource func bool string u8 u32 s64 f64 list option result tuple borrow own"))

;;; --- Peek token test ---

(let ((lx (make-string-lexer "a b c")))
  (check "peek: returns first token"
         '(id "a")
         (wit-peek-token lx))
  (check "peek: same token again"
         '(id "a")
         (wit-peek-token lx))
  (check "next: consumes peeked"
         '(id "a")
         (wit-next-token lx))
  (check "next: advances"
         '(id "b")
         (wit-next-token lx)))

;;; --- Type parser tests ---

(define (parse-type-from str)
  (let ((lx (make-string-lexer str)))
    (parse-wit-type lx)))

(check "type: u8"    '(prim "u8")    (parse-type-from "u8"))
(check "type: u16"   '(prim "u16")   (parse-type-from "u16"))
(check "type: u32"   '(prim "u32")   (parse-type-from "u32"))
(check "type: u64"   '(prim "u64")   (parse-type-from "u64"))
(check "type: s8"    '(prim "s8")    (parse-type-from "s8"))
(check "type: s16"   '(prim "s16")   (parse-type-from "s16"))
(check "type: s32"   '(prim "s32")   (parse-type-from "s32"))
(check "type: s64"   '(prim "s64")   (parse-type-from "s64"))
(check "type: f32"   '(prim "f32")   (parse-type-from "f32"))
(check "type: f64"   '(prim "f64")   (parse-type-from "f64"))
(check "type: bool"  '(prim "bool")  (parse-type-from "bool"))
(check "type: char"  '(prim "char")  (parse-type-from "char"))
(check "type: string" '(prim "string") (parse-type-from "string"))

(check "type: list<u32>"
       '(list (prim "u32"))
       (parse-type-from "list<u32>"))

(check "type: option<string>"
       '(option (prim "string"))
       (parse-type-from "option<string>"))

(check "type: tuple<u32, string, bool>"
       '(tuple (prim "u32") (prim "string") (prim "bool"))
       (parse-type-from "tuple<u32, string, bool>"))

(check "type: result<string, u32>"
       '(result (prim "string") (prim "u32"))
       (parse-type-from "result<string, u32>"))

(check "type: result<string>"
       '(result (prim "string") #f)
       (parse-type-from "result<string>"))

(check "type: result (bare)"
       '(result #f #f)
       (parse-type-from "result"))

(check "type: result<_, u32>"
       '(result #f (prim "u32"))
       (parse-type-from "result<_, u32>"))

(check "type: borrow<blob>"
       '(borrow "blob")
       (parse-type-from "borrow<blob>"))

(check "type: user ref"
       '(ref "my-type")
       (parse-type-from "my-type"))

(check "type: nested list<option<u32>>"
       '(list (option (prim "u32")))
       (parse-type-from "list<option<u32>>"))

;;; --- Func type parser tests ---

(define (parse-functype-from str)
  (let ((lx (make-string-lexer str)))
    (parse-wit-func-type lx)))

(check "functype: no params, no result"
       '(() . #f)
       (parse-functype-from "func()"))

(check "functype: one param"
       '((("name" . (prim "string"))) . #f)
       (parse-functype-from "func(name: string)"))

(check "functype: params and result"
       '((("a" . (prim "u32")) ("b" . (prim "u32"))) . (prim "u32"))
       (parse-functype-from "func(a: u32, b: u32) -> u32"))

(check "functype: trailing comma in params"
       '((("x" . (prim "f32")) ("y" . (prim "f32"))) . #f)
       (parse-functype-from "func(x: f32, y: f32,)"))

;;; --- Gate parser tests ---

(define (parse-gate-from str)
  (let ((lx (make-string-lexer str)))
    (parse-wit-gate lx)))

(check "gate: none"
       #f
       (parse-gate-from "func"))

(check "gate: @since"
       '((since "1.1.0"))
       (parse-gate-from "@since(version = 1.1.0) func"))

(check "gate: @unstable"
       '((unstable "experimental"))
       (parse-gate-from "@unstable(feature = experimental) func"))

(check "gate: @since + @deprecated"
       '((since "1.0.0") (deprecated "2.0.0"))
       (parse-gate-from "@since(version = 1.0.0) @deprecated(version = 2.0.0) func"))

;;; --- Full file parse tests ---

(define (test-parse-file path)
  (parse-wit-file path))

;; Trivial
(let ((pkg (test-parse-file "tests/wit/trivial.wit")))
  (check "trivial: namespace" "test" (wit-package-namespace pkg))
  (check "trivial: name" "trivial" (wit-package-name pkg))
  (check "trivial: version" "1.0.0" (wit-package-version pkg))
  (check "trivial: one interface"
         1 (length (wit-package-interfaces pkg)))
  (check "trivial: one world"
         1 (length (wit-package-worlds pkg)))
  (let ((iface (car (wit-package-interfaces pkg))))
    (check "trivial: iface name" "greeter" (wit-interface-name iface))
    (check "trivial: iface has one item"
           1 (length (wit-interface-items iface)))
    (let ((f (car (wit-interface-items iface))))
      (check "trivial: func name" "greet" (wit-func-name f))
      (check "trivial: func params"
             '(("name" . (prim "string")))
             (wit-func-params f))
      (check "trivial: func result"
             '(prim "string")
             (wit-func-result f))))
  (let ((w (car (wit-package-worlds pkg))))
    (check "trivial: world name" "hello" (wit-world-name w))
    (check "trivial: world has one item"
           1 (length (wit-world-items w)))))

;; Comments
(let ((pkg (test-parse-file "tests/wit/comments.wit")))
  (check "comments: parses ok" "test" (wit-package-namespace pkg))
  (check "comments: one interface"
         1 (length (wit-package-interfaces pkg)))
  (let ((iface (car (wit-package-interfaces pkg))))
    (check "comments: iface name" "commented" (wit-interface-name iface))
    (check "comments: one func"
           1 (length (wit-interface-items iface)))))

;; Types
(let ((pkg (test-parse-file "tests/wit/types.wit")))
  (check "types: namespace" "test" (wit-package-namespace pkg))
  (check "types: one interface"
         1 (length (wit-package-interfaces pkg)))
  (let* ((iface (car (wit-package-interfaces pkg)))
         (items (wit-interface-items iface)))
    (check "types: iface name" "all-types" (wit-interface-name iface))
    ;; type alias, record, variant, enum, flags, resource, 2 funcs = 8 items
    (check "types: 8 items" 8 (length items))
    ;; Type alias
    (let ((ta (list-ref items 0)))
      (check "types: alias name" "my-int" (wit-type-alias-name ta))
      (check "types: alias type" '(prim "u32") (wit-type-alias-type ta)))
    ;; Record
    (let ((rec (list-ref items 1)))
      (check "types: record name" "point" (wit-record-name rec))
      (check "types: record fields"
             '(("x" . (prim "f32")) ("y" . (prim "f32")))
             (wit-record-fields rec)))
    ;; Variant
    (let ((var (list-ref items 2)))
      (check "types: variant name" "filter" (wit-variant-name var))
      (check "types: variant cases"
             '(("all" . #f) ("none" . #f) ("some" . (list (prim "string"))))
             (wit-variant-cases var)))
    ;; Enum
    (let ((en (list-ref items 3)))
      (check "types: enum name" "color" (wit-enum-name en))
      (check "types: enum cases" '("red" "green" "blue") (wit-enum-cases en)))
    ;; Flags
    (let ((fl (list-ref items 4)))
      (check "types: flags name" "permissions" (wit-flags-name fl))
      (check "types: flags fields" '("read" "write" "execute") (wit-flags-fields fl)))
    ;; Resource
    (let ((res (list-ref items 5)))
      (check "types: resource name" "blob" (wit-resource-name res))
      (check "types: resource 4 methods" 4 (length (wit-resource-methods res)))
      (let ((ctor (list-ref (wit-resource-methods res) 0)))
        (check "types: constructor name" "constructor" (wit-func-name ctor))
        (check "types: constructor params"
               '(("init" . (list (prim "u8"))))
               (wit-func-params ctor))))))

;; Gates
(let ((pkg (test-parse-file "tests/wit/gates.wit")))
  (check "gates: two interfaces"
         2 (length (wit-package-interfaces pkg)))
  (let* ((iface (car (wit-package-interfaces pkg)))
         (items (wit-interface-items iface)))
    (check "gates: 4 items" 4 (length items))
    ;; stable-func has no gate
    (check "gates: stable no gate" #f (wit-func-gate (list-ref items 0)))
    ;; newer-func has @since
    (check "gates: since gate"
           '((since "1.1.0"))
           (wit-func-gate (list-ref items 1)))
    ;; experimental has @unstable
    (check "gates: unstable gate"
           '((unstable "experimental"))
           (wit-func-gate (list-ref items 2)))
    ;; old-func has @since + @deprecated
    (check "gates: since+deprecated"
           '((since "1.0.0") (deprecated "2.0.0"))
           (wit-func-gate (list-ref items 3))))
  ;; Second interface has gate
  (let ((iface2 (list-ref (wit-package-interfaces pkg) 1)))
    (check "gates: interface gate"
           '((since "1.1.0"))
           (wit-interface-gate iface2))))

;; World
(let ((pkg (test-parse-file "tests/wit/world.wit")))
  (check "world: 2 interfaces" 2 (length (wit-package-interfaces pkg)))
  (check "world: 1 world" 1 (length (wit-package-worlds pkg)))
  (let* ((w (car (wit-package-worlds pkg)))
         (items (wit-world-items w)))
    (check "world: name" "my-app" (wit-world-name w))
    ;; 3 imports, 3 exports, 2 includes = 8 items
    (check "world: 8 items" 8 (length items))
    ;; First import: func
    (let ((imp (list-ref items 0)))
      (check "world: import func tag" 'import (car imp))
      (check "world: import func name" "log" (cadr imp)))
    ;; Second import: local path
    (let ((imp (list-ref items 1)))
      (check "world: import local tag" 'import-path (car imp))
      (check "world: import local path" '(local "logger") (cadr imp)))
    ;; Third import: package path
    (let ((imp (list-ref items 2)))
      (check "world: import pkg tag" 'import-path (car imp))
      (check "world: import pkg path"
             '(package "wasi" "cli" "stdout" "0.2.0")
             (cadr imp)))
    ;; Fourth: export func
    (let ((exp (list-ref items 3)))
      (check "world: export func tag" 'export (car exp))
      (check "world: export func name" "render" (cadr exp)))
    ;; Fifth: export local
    (let ((exp (list-ref items 4)))
      (check "world: export local tag" 'export-path (car exp))
      (check "world: export local path" '(local "renderer") (cadr exp)))
    ;; Sixth: export package path
    (let ((exp (list-ref items 5)))
      (check "world: export pkg tag" 'export-path (car exp))
      (check "world: export pkg path"
             '(package "wasi" "http" "handler" "0.2.0")
             (cadr exp)))
    ;; Include
    (let ((inc (list-ref items 6)))
      (check "world: include tag" 'include (car inc))
      (check "world: include path" '(local "other-world") (cadr inc))
      (check "world: include no renames" '() (caddr inc)))
    ;; Include with renames
    (let ((inc (list-ref items 7)))
      (check "world: include-with tag" 'include (car inc))
      (check "world: include-with renames"
             '(("foo" . "bar") ("baz" . "qux"))
             (caddr inc)))))

;;; --- Multi-file merge test ---

(let ((pkg (parse-wit-package
            (list "tests/wit/trivial.wit" "tests/wit/comments.wit"))))
  (check "merge: namespace from first" "test" (wit-package-namespace pkg))
  (check "merge: name from first" "trivial" (wit-package-name pkg))
  (check "merge: merged interfaces" 2 (length (wit-package-interfaces pkg)))
  (check "merge: merged worlds" 1 (length (wit-package-worlds pkg))))

;;; --- Done ---

(report)
