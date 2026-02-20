;; skip: true
;; expect:
;; expect: 28 passed, 0 failed

(define (string-hash s)
  (let ((len (string-length s)))
    (let loop ((i 0) (h 0))
      (if (>= i len) (bitwise-and h 63)
          (loop (+ i 1) (+ h (char->integer (string-ref s i))))))))
(define (make-string-ht) (make-vector 64 '()))
(define (string-ht-set! ht key val)
  (let ((idx (string-hash key)))
    (vector-set! ht idx (cons (cons key val) (vector-ref ht idx)))))
(define (string-ht-has? ht key)
  (let ((idx (string-hash key)))
    (let loop ((b (vector-ref ht idx)))
      (cond ((null? b) #f) ((string=? (caar b) key) #t) (else (loop (cdr b)))))))

(include "../src/compiler/wit.scm")

(define *pass* 0)
(define *fail* 0)
(define (check name expected actual)
  (if (equal? expected actual)
      (set! *pass* (+ *pass* 1))
      (begin
        (set! *fail* (+ *fail* 1))
        (display "FAIL: ") (display name) (newline)
        (display "  expected: ") (write expected) (newline)
        (display "  actual:   ") (write actual) (newline))))

(define (make-string-lexer str)
  (make-wit-lexer (open-input-string str) "<test>"))

;;; --- Type parser tests ---

(define (parse-type-from str)
  (let ((lx (make-string-lexer str)))
    (parse-wit-type lx)))

(check "type: u8" '(prim "u8") (parse-type-from "u8"))
(check "type: u16" '(prim "u16") (parse-type-from "u16"))
(check "type: u32" '(prim "u32") (parse-type-from "u32"))
(check "type: u64" '(prim "u64") (parse-type-from "u64"))
(check "type: s8" '(prim "s8") (parse-type-from "s8"))
(check "type: s16" '(prim "s16") (parse-type-from "s16"))
(check "type: s32" '(prim "s32") (parse-type-from "s32"))
(check "type: s64" '(prim "s64") (parse-type-from "s64"))
(check "type: f32" '(prim "f32") (parse-type-from "f32"))
(check "type: f64" '(prim "f64") (parse-type-from "f64"))
(check "type: bool" '(prim "bool") (parse-type-from "bool"))
(check "type: char" '(prim "char") (parse-type-from "char"))
(check "type: string" '(prim "string") (parse-type-from "string"))
(check "type: list<u32>" '(list (prim "u32")) (parse-type-from "list<u32>"))
(check "type: option<string>" '(option (prim "string")) (parse-type-from "option<string>"))
(check "type: tuple<u32, string, bool>" '(tuple (prim "u32") (prim "string") (prim "bool")) (parse-type-from "tuple<u32, string, bool>"))
(check "type: result<string, u32>" '(result (prim "string") (prim "u32")) (parse-type-from "result<string, u32>"))
(check "type: result<string>" '(result (prim "string") #f) (parse-type-from "result<string>"))
(check "type: result (bare)" '(result #f #f) (parse-type-from "result"))
(check "type: result<_, u32>" '(result #f (prim "u32")) (parse-type-from "result<_, u32>"))
(check "type: borrow<blob>" '(borrow "blob") (parse-type-from "borrow<blob>"))
(check "type: user ref" '(ref "my-type") (parse-type-from "my-type"))
(check "type: nested" '(list (option (prim "u32"))) (parse-type-from "list<option<u32>>"))

;;; --- Func type parser tests ---

(define (parse-functype-from str)
  (let ((lx (make-string-lexer str)))
    (parse-wit-func-type lx)))

(check "functype: no params, no result" '(() . #f) (parse-functype-from "func()"))
(check "functype: one param" '((("name" . (prim "string"))) . #f) (parse-functype-from "func(name: string)"))
(check "functype: params and result" '((("a" . (prim "u32")) ("b" . (prim "u32"))) . (prim "u32")) (parse-functype-from "func(a: u32, b: u32) -> u32"))
(check "functype: trailing comma" '((("x" . (prim "f32")) ("y" . (prim "f32"))) . #f) (parse-functype-from "func(x: f32, y: f32,)"))

;;; --- Gate parser tests ---

(define (parse-gate-from str)
  (let ((lx (make-string-lexer str)))
    (parse-wit-gate lx)))

(check "gate: none" #f (parse-gate-from "func"))
(check "gate: @since" '((since "1.1.0")) (parse-gate-from "@since(version = 1.1.0) func"))
(check "gate: @unstable" '((unstable "experimental")) (parse-gate-from "@unstable(feature = experimental) func"))
(check "gate: @since + @deprecated" '((since "1.0.0") (deprecated "2.0.0")) (parse-gate-from "@since(version = 1.0.0) @deprecated(version = 2.0.0) func"))

(newline)
(display *pass*) (display " passed, ") (display *fail*) (display " failed") (newline)
(if (> *fail* 0) (exit 1))
