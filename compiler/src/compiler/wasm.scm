;;; wasm.scm — WASM byte buffer, LEB128 encoding, and constants
;;; Parallel Scheme implementation of wasm.c + wasm_ops.h for Phase 5 codegen

;;;---------------------------------------------------------------------------
;;; A. Constants (transcribed from wasm_ops.h)
;;;---------------------------------------------------------------------------

;; WASM header: \0asm version 1
(define WASM-HEADER '(#x00 #x61 #x73 #x6D #x01 #x00 #x00 #x00))

;; Section IDs
(define SEC-TYPE     1)
(define SEC-IMPORT   2)
(define SEC-FUNCTION 3)
(define SEC-TABLE    4)
(define SEC-MEMORY   5)
(define SEC-GLOBAL   6)
(define SEC-EXPORT   7)
(define SEC-ELEMENT  9)
(define SEC-CODE     10)
(define SEC-TAG      13)  ;; Exception tag section (exception handling proposal)

;; Opcodes
(define OP-BLOCK      #x02)
(define OP-LOOP       #x03)
(define OP-IF         #x04)
(define OP-ELSE       #x05)
(define OP-THROW      #x08)  ;; Exception handling: throw
(define OP-TRY-TABLE  #x1F)  ;; Exception handling: try_table (new EH spec)
(define OP-END        #x0B)
(define OP-BR         #x0C)
(define OP-BR-IF      #x0D)
(define OP-BR-TABLE   #x0E)
(define OP-RETURN     #x0F)
(define OP-CALL          #x10)
(define OP-CALL-INDIRECT #x11)
(define OP-RETURN-CALL          #x12)
(define OP-RETURN-CALL-INDIRECT #x13)
(define OP-DROP       #x1A)
(define OP-LOCAL-GET  #x20)
(define OP-LOCAL-SET  #x21)
(define OP-LOCAL-TEE  #x22)
(define OP-GLOBAL-GET #x23)
(define OP-GLOBAL-SET #x24)
(define OP-I32-LOAD    #x28)
(define OP-I64-LOAD    #x29)
(define OP-I32-LOAD8-U #x2D)
(define OP-I32-STORE  #x36)
(define OP-F64-STORE  #x39)
(define OP-I32-STORE8 #x3A)
(define OP-MEMORY-SIZE #x3F)
(define OP-MEMORY-GROW #x40)
(define OP-BULK-PREFIX    #xFC)
(define BULK-MEMORY-COPY  #x0A)
(define BULK-MEMORY-FILL  #x0B)
(define OP-I32-CONST  #x41)
(define OP-I32-EQZ    #x45)
(define OP-I32-EQ     #x46)
(define OP-I32-NE     #x47)
(define OP-I32-LT-S   #x48)
(define OP-I32-LT-U   #x49)
(define OP-I32-GT-S   #x4A)
(define OP-I32-LE-S   #x4C)
(define OP-I32-GE-S   #x4E)
(define OP-I32-GE-U   #x4F)
(define OP-I32-ADD    #x6A)
(define OP-I32-SUB    #x6B)
(define OP-I32-MUL    #x6C)
(define OP-I32-DIV-S  #x6D)
(define OP-I32-DIV-U  #x6E)
(define OP-I32-REM-S  #x6F)
(define OP-I32-REM-U  #x70)
(define OP-I32-AND    #x71)
(define OP-I32-OR     #x72)
(define OP-I32-XOR    #x73)
(define OP-I32-SHL    #x74)
(define OP-I32-SHR-S  #x75)

;; f64 opcodes
(define OP-F64-CONST       #x44)
(define OP-F64-EQ          #x61)
(define OP-F64-NE          #x62)
(define OP-F64-LT          #x63)
(define OP-F64-GT          #x64)
(define OP-F64-LE          #x65)
(define OP-F64-GE          #x66)
(define OP-F64-ABS         #x99)
(define OP-F64-NEG         #x9A)
(define OP-F64-CEIL        #x9B)
(define OP-F64-FLOOR       #x9C)
(define OP-F64-TRUNC       #x9D)
(define OP-F64-NEAREST     #x9E)
(define OP-F64-SQRT        #x9F)
(define OP-F64-ADD         #xA0)
(define OP-F64-SUB         #xA1)
(define OP-F64-MUL         #xA2)
(define OP-F64-DIV         #xA3)
(define OP-F64-COPYSIGN    #xA6)
(define OP-I32-TRUNC-F64-S #xAA)

;; i64 opcodes
(define OP-I64-CONST       #x42)
(define OP-I64-ADD         #x7C)
(define OP-I64-SUB         #x7D)
(define OP-I64-AND         #x83)
(define OP-I64-OR          #x84)
(define OP-I64-SHL         #x86)
(define OP-I64-SHR-U       #x88)
(define OP-I32-WRAP-I64    #xA7)
(define OP-I64-EXTEND-I32-S #xAC)
(define OP-I64-EXTEND-I32-U #xAD)
(define OP-F64-CONVERT-I32-S #xB7)
(define OP-F64-CONVERT-I64-S #xB9)
(define OP-I64-TRUNC-F64-S  #xAE)
(define OP-I64-REINTERPRET-F64 #xBD)
(define OP-F64-REINTERPRET-I64 #xBF)

;; GC prefix + ref opcodes
(define OP-GC-PREFIX  #xFB)
(define OP-REF-NULL   #xD0)
(define OP-REF-IS-NULL #xD1)
(define OP-REF-EQ     #xD3)

;; Type constructors
(define TYPE-FUNC    #x60)
(define TYPE-I64     #x7E)
(define TYPE-F64     #x7C)
(define TYPE-F32     #x7D)
(define TYPE-I32     #x7F)
(define TYPE-VOID    #x40)
(define TYPE-FUNCREF #x70)

;; GC opcodes (follow 0xFB prefix)
(define GC-STRUCT-NEW      #x00)
(define GC-STRUCT-GET      #x02)
(define GC-STRUCT-SET      #x05)
(define GC-ARRAY-NEW         #x06)
(define GC-ARRAY-NEW-DEFAULT #x07)
(define GC-ARRAY-NEW-FIXED   #x08)
(define GC-ARRAY-GET       #x0B)
(define GC-ARRAY-GET-U     #x0D)
(define GC-ARRAY-SET       #x0E)
(define GC-ARRAY-LEN       #x0F)
(define GC-REF-TEST-NN     #x14)
(define GC-REF-TEST        #x15)
(define GC-REF-CAST-NULL   #x16)
(define GC-REF-CAST        #x17)
(define GC-BR-ON-CAST      #x18)
(define GC-BR-ON-CAST-FAIL #x19)
(define GC-REF-I31         #x1C)
(define GC-I31-GET-S       #x1D)

;; Heap type constants
(define HT-EQ     #x6D)
(define HT-I31    #x6C)
(define HT-STRUCT #x6B)
(define HT-ARRAY  #x6A)
(define HT-NONE   #x65)

;; Composite type constructors
(define COMP-ARRAY  #x5E)
(define COMP-STRUCT #x5F)

;; Packed storage types
(define PACKED-I8 #x78)

;; Field mutability
(define FIELD-CONST #x00)
(define FIELD-MUT   #x01)

;; Reference type constructors
(define REF-TYPE      #x64)
(define REF-NULL-TYPE #x63)

;; Fixed type indices
(define TY-ENV         0)
(define TY-CLOSURE     1)
(define TY-I32-VOID    2)
(define TY-VOID-VOID   3)
(define TY-IO-IMPORT   4)
(define TY-PAIR        5)
(define TY-STRING      6)
(define TY-EQ-VOID     7)
(define TY-FIXED-COUNT 8)

;;;---------------------------------------------------------------------------
;;; B. Component Model binary format constants
;;;---------------------------------------------------------------------------

;; Component header: \0asm version 13, layer 1
(define COMPONENT-HEADER '(#x00 #x61 #x73 #x6D #x0D #x00 #x01 #x00))

;; Component section IDs
(define CSEC-CORE-MODULE       1)
(define CSEC-CORE-INSTANCE     2)
(define CSEC-CORE-TYPE         3)
(define CSEC-COMPONENT-INSTANCE 5)
(define CSEC-ALIAS             6)
(define CSEC-TYPE              7)
(define CSEC-CANON             8)
(define CSEC-IMPORT           10)
(define CSEC-EXPORT           11)

;; Canonical opcodes
(define CANON-LIFT   #x00)   ;; followed by 0x00
(define CANON-LOWER  #x01)   ;; followed by 0x00
(define CANON-RESOURCE-DROP #x03)

;; Canonical options
(define CANONOPT-UTF8      #x00)
(define CANONOPT-MEMORY    #x03)
(define CANONOPT-REALLOC   #x04)

;; Component value types (signed LEB128, encoded as unsigned bytes)
(define CVT-BOOL    #x7F)
(define CVT-S8      #x7E)
(define CVT-U8      #x7D)
(define CVT-S16     #x7C)
(define CVT-U16     #x7B)
(define CVT-S32     #x7A)
(define CVT-U32     #x79)
(define CVT-S64     #x78)
(define CVT-U64     #x77)
(define CVT-F32     #x76)
(define CVT-F64     #x75)
(define CVT-CHAR    #x74)
(define CVT-STRING  #x73)

;; Composite value type constructors
(define CVT-RECORD  #x72)
(define CVT-VARIANT #x71)
(define CVT-LIST    #x70)
(define CVT-TUPLE   #x6F)
(define CVT-FLAGS   #x6E)
(define CVT-ENUM    #x6D)
(define CVT-OPTION  #x6B)
(define CVT-RESULT  #x6A)
(define CVT-OWN     #x69)
(define CVT-BORROW  #x68)

;; Component type constructors
(define CT-FUNC     #x40)
(define CT-INSTANCE #x42)

;; Component extern descriptors
(define CEXT-CORE-MODULE  #x00)  ;; followed by 0x11
(define CEXT-FUNC         #x01)
(define CEXT-TYPE         #x03)
(define CEXT-COMPONENT    #x04)
(define CEXT-INSTANCE     #x05)

;; Core sort bytes (used in aliases and instantiation args)
(define CSORT-FUNC     #x00)
(define CSORT-MEMORY   #x02)
(define CSORT-INSTANCE #x12)

;; Instance declaration kinds (inside instance type)
(define IDECL-CORE-TYPE  #x00)
(define IDECL-TYPE       #x01)
(define IDECL-ALIAS      #x02)
(define IDECL-EXPORT     #x04)

;; Alias target discriminants
(define ALIAS-EXPORT      #x00)
(define ALIAS-CORE-EXPORT #x01)
(define ALIAS-OUTER       #x02)

;;;---------------------------------------------------------------------------
;;; C. Byte buffer
;;;---------------------------------------------------------------------------
;;; Buffer is a 3-element vector: #(bytevector length capacity)

(define wbuf-init-cap 256)

(define (wbuf-make)
  (vector (make-bytevector wbuf-init-cap 0) 0 wbuf-init-cap))

(define (wbuf-data buf) (vector-ref buf 0))
(define (wbuf-len buf)  (vector-ref buf 1))
(define (wbuf-cap buf)  (vector-ref buf 2))

(define (wbuf-set-data! buf v) (vector-set! buf 0 v))
(define (wbuf-set-len!  buf n) (vector-set! buf 1 n))
(define (wbuf-set-cap!  buf n) (vector-set! buf 2 n))

(define (wbuf-ensure! buf need)
  (let ((len (wbuf-len buf))
        (cap (wbuf-cap buf)))
    (when (> (+ len need) cap)
      (let loop ((new-cap cap))
        (if (< new-cap (+ len need))
            (loop (* new-cap 2))
            (let ((new-data (make-bytevector new-cap 0)))
              (bytevector-copy! new-data 0 (wbuf-data buf) 0 len)
              (wbuf-set-data! buf new-data)
              (wbuf-set-cap!  buf new-cap)))))))

(define (wbuf-byte! buf v)
  (let ((len (vector-ref buf 1))
        (cap (vector-ref buf 2)))
    (when (>= len cap)
      (wbuf-ensure! buf 1))
    (bytevector-u8-set! (vector-ref buf 0) len (bitwise-and v #xFF))
    (vector-set! buf 1 (+ len 1))))

(define (wbuf-bytes! buf u8v off n)
  (wbuf-ensure! buf n)
  (let ((pos (wbuf-len buf)))
    (bytevector-copy! (wbuf-data buf) pos u8v off (+ off n))
    (wbuf-set-len! buf (+ pos n))))

(define (wbuf-bytes-list! buf lst)
  (for-each (lambda (b) (wbuf-byte! buf b)) lst))

(define (wbuf-func-body! sec body)
  (wbuf-u32! sec (wbuf-len body))
  (wbuf-bytes! sec (wbuf-data body) 0 (wbuf-len body)))

(define (wbuf-u32! buf v)
  ;; Unsigned LEB128 — inlined writes, single ensure
  (wbuf-ensure! buf 5)
  (let ((data (vector-ref buf 0))
        (pos (vector-ref buf 1)))
    (let loop ((v v) (p pos))
      (let ((byte (bitwise-and v #x7F))
            (rest (arithmetic-shift v -7)))
        (if (> rest 0)
            (begin
              (bytevector-u8-set! data p (bitwise-ior byte #x80))
              (loop rest (+ p 1)))
            (begin
              (bytevector-u8-set! data p byte)
              (vector-set! buf 1 (+ p 1))))))))

(define (wbuf-i32! buf v)
  ;; Signed LEB128 — inlined writes, single ensure
  (wbuf-ensure! buf 5)
  (let ((data (vector-ref buf 0))
        (pos (vector-ref buf 1)))
    (let loop ((v v) (p pos))
      (let* ((byte (bitwise-and v #x7F))
             (rest (arithmetic-shift v -7))
             (sign-bit (bitwise-and byte #x40)))
        (if (or (and (= rest 0) (= sign-bit 0))
                (and (= rest -1) (not (= sign-bit 0))))
            (begin
              (bytevector-u8-set! data p byte)
              (vector-set! buf 1 (+ p 1)))
            (begin
              (bytevector-u8-set! data p (bitwise-ior byte #x80))
              (loop rest (+ p 1))))))))

(define (wbuf-i64! buf v)
  ;; Signed LEB128 (same algorithm, Scheme integers are arbitrary precision)
  (wbuf-i32! buf v))

(define (wbuf-f64! buf v)
  (wbuf-ensure! buf 8)
  (bytevector-f64-native-set! (wbuf-data buf) (wbuf-len buf) v)
  (wbuf-set-len! buf (+ (wbuf-len buf) 8)))

(define (wbuf-name! buf s)
  (let ((len (string-length s)))
    (wbuf-u32! buf len)
    (let loop ((i 0))
      (when (< i len)
        (wbuf-byte! buf (char->integer (string-ref s i)))
        (loop (+ i 1))))))

(define (wbuf-section! out id contents)
  (wbuf-byte! out id)
  (wbuf-u32! out (wbuf-len contents))
  (wbuf-bytes! out (wbuf-data contents) 0 (wbuf-len contents)))

(define (wbuf-reset! buf)
  (wbuf-set-len! buf 0))

(define (wbuf->bytevector buf)
  (let* ((len (wbuf-len buf))
         (result (make-bytevector len)))
    (bytevector-copy! result 0 (wbuf-data buf) 0 len)
    result))
