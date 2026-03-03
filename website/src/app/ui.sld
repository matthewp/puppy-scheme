(define-library (app ui)
  (export
    ;; Component system
    define-component
    create-instance render-instance dispatch-instance
    ;; HTML macro
    html
    ;; Rendering
    render-to-string
    ;; Serialization
    serialize-opcodes)
  (include "../html.scm")
  (include "../component.scm")
  (begin
    ;;; Binary opcode serialization

    ;; Opcode tag values (32-byte records)
    (define TAG-OPEN 0)
    (define TAG-CLOSE 1)
    (define TAG-ATTR 2)
    (define TAG-TEXT 3)
    (define TAG-SLOT 4)
    (define TAG-EVENT 5)
    (define TAG-ATTR-SLOT 6)
    (define TAG-COMPONENT 7)

    ;; Return area for (array-ptr, count) pair
    (define ret-area (make-bytevector 8))

    ;; Copy a GC string into linear memory, return (ptr . len)
    (define (string->mem str)
      (let* ((len (string-length str))
             (bv (make-bytevector len)))
        (bytevector-copy-string! bv 0 str)
        (cons (bytevector->pointer bv) len)))

    (define no-string (cons 0 0))

    ;; Write one 32-byte opcode record into bytevector
    (define (write-opcode! bv idx tag f0 f1)
      (let ((off (* idx 32)))
        (bytevector-u8-set! bv off tag)
        (bytevector-u32-native-set! bv (+ off 8) (car f0))
        (bytevector-u32-native-set! bv (+ off 12) (cdr f0))
        (bytevector-u32-native-set! bv (+ off 16) (car f1))
        (bytevector-u32-native-set! bv (+ off 20) (cdr f1))))

    ;; Walk opcode list, write 32-byte records
    (define (write-opcodes! bv idx opcodes)
      (if (null? opcodes) idx
          (let ((op (car opcodes))
                (rest (cdr opcodes)))
            (let ((type (car op)))
              (if (eq? type 'open)
                  (begin (write-opcode! bv idx TAG-OPEN (string->mem (cadr op)) no-string)
                         (write-opcodes! bv (+ idx 1) rest))
              (if (eq? type 'close)
                  (begin (write-opcode! bv idx TAG-CLOSE no-string no-string)
                         (write-opcodes! bv (+ idx 1) rest))
              (if (eq? type 'text)
                  (begin (write-opcode! bv idx TAG-TEXT (string->mem (cadr op)) no-string)
                         (write-opcodes! bv (+ idx 1) rest))
              (if (eq? type 'slot)
                  (begin (write-opcode! bv idx TAG-SLOT (string->mem (cadr op)) no-string)
                         (write-opcodes! bv (+ idx 1) rest))
              (if (eq? type 'attr)
                  (begin (write-opcode! bv idx TAG-ATTR (string->mem (cadr op)) (string->mem (caddr op)))
                         (write-opcodes! bv (+ idx 1) rest))
              (if (eq? type 'event)
                  (begin (write-opcode! bv idx TAG-EVENT (string->mem (cadr op)) (string->mem (caddr op)))
                         (write-opcodes! bv (+ idx 1) rest))
              (if (eq? type 'attr-slot)
                  (begin (write-opcode! bv idx TAG-ATTR-SLOT (string->mem (cadr op)) (string->mem (caddr op)))
                         (write-opcodes! bv (+ idx 1) rest))
              (if (eq? type 'component)
                  (begin (write-opcode! bv idx TAG-COMPONENT (cons (cadr op) 0) no-string)
                         (write-opcodes! bv (+ idx 1) rest))
                  (write-opcodes! bv idx rest)))))))))))))

    ;; Serialize opcode list -> pointer to (array-ptr, count) pair
    (define (serialize-opcodes opcodes)
      (let* ((n (length opcodes))
             (bv (make-bytevector (* n 32))))
        (write-opcodes! bv 0 opcodes)
        (bytevector-u32-native-set! ret-area 0 (bytevector->pointer bv))
        (bytevector-u32-native-set! ret-area 4 n)
        (bytevector->pointer ret-area)))))
