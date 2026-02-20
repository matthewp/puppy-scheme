;;; system.scm — System and bytevector runtime functions

;; command-line: returns list of argument strings
;; Uses P2 get-arguments import (canonical ABI: retptr → {data_ptr, count})
(define rt-command-line
  '(()
    ("i" "ptr" "count" "str_ptr" "str_len" "result" "cur_str" "j")
    ((%call-get-arguments 0)
     (set! ptr (%mem-load32 0))
     (set! count (%mem-load32 4))
     (set! result '())
     (set! i (%i31-sub count 1))
     (%block-void
       (%loop-void
         (%br-if 1 (%i31-lt i 0))
         (set! str_ptr (%mem-load32 (%i31-add ptr (%i31-shl i 3))))
         (set! str_len (%mem-load32 (%i31-add ptr (%i31-add (%i31-shl i 3) 4))))
         (set! cur_str (%make-string str_len 0))
         (set! j 0)
         (%block-void
           (%loop-void
             (%br-if 1 (%i31-ge j str_len))
             (%string-set! cur_str j (%mem-load8 (%i31-add str_ptr j)))
             (set! j (%i31-add j 1))
             (%br 0)))
         (set! result (cons cur_str result))
         (set! i (%i31-sub i 1))
         (%br 0)))
     result)))

;; get-environment-variable: name (GC string) → value string or #f
;; Uses get-environment import, scans for matching key
(define rt-get-env-var
  '(("name")
    ("i" "ptr" "count" "key_ptr" "key_len" "val_ptr" "val_len" "name_len" "j" "match" "result")
    ((%call-get-environment 0)
     (set! ptr (%mem-load32 0))
     (set! count (%mem-load32 4))
     (set! name_len (%string-length name))
     (set! result #f)
     (set! i 0)
     (%block-void
       (%loop-void
         (%br-if 1 (%i31-ge i count))
         (set! key_ptr (%mem-load32 (%i31-add ptr (%i31-shl i 4))))
         (set! key_len (%mem-load32 (%i31-add ptr (%i31-add (%i31-shl i 4) 4))))
         (if (%i31-eq key_len name_len)
             (begin
               (set! j 0)
               (set! match 1)
               (%block-void
                 (%loop-void
                   (%br-if 1 (%i31-ge j key_len))
                   (if (%i31-eq (%mem-load8 (%i31-add key_ptr j))
                                (%string-ref name j))
                       0
                       (begin (set! match 0) (%br 1)))
                   (set! j (%i31-add j 1))
                   (%br 0)))
               (if match
                   (begin
                     (set! val_ptr (%mem-load32 (%i31-add ptr (%i31-add (%i31-shl i 4) 8))))
                     (set! val_len (%mem-load32 (%i31-add ptr (%i31-add (%i31-shl i 4) 12))))
                     (set! result (%make-string val_len 0))
                     (set! j 0)
                     (%block-void
                       (%loop-void
                         (%br-if 1 (%i31-ge j val_len))
                         (%string-set! result j (%mem-load8 (%i31-add val_ptr j)))
                         (set! j (%i31-add j 1))
                         (%br 0)))
                     (%br 2))
                   0))
             0)
         (set! i (%i31-add i 1))
         (%br 0)))
     result)))

;; get-environment-variables: returns alist of (key . value) pairs
(define rt-get-env-vars
  '(()
    ("i" "ptr" "count" "key_ptr" "key_len" "val_ptr" "val_len" "j" "result" "key" "val")
    ((%call-get-environment 0)
     (set! ptr (%mem-load32 0))
     (set! count (%mem-load32 4))
     (set! result '())
     (set! i (%i31-sub count 1))
     (%block-void
       (%loop-void
         (%br-if 1 (%i31-lt i 0))
         (set! key_ptr (%mem-load32 (%i31-add ptr (%i31-shl i 4))))
         (set! key_len (%mem-load32 (%i31-add ptr (%i31-add (%i31-shl i 4) 4))))
         (set! val_ptr (%mem-load32 (%i31-add ptr (%i31-add (%i31-shl i 4) 8))))
         (set! val_len (%mem-load32 (%i31-add ptr (%i31-add (%i31-shl i 4) 12))))
         (set! key (%make-string key_len 0))
         (set! j 0)
         (%block-void
           (%loop-void
             (%br-if 1 (%i31-ge j key_len))
             (%string-set! key j (%mem-load8 (%i31-add key_ptr j)))
             (set! j (%i31-add j 1))
             (%br 0)))
         (set! val (%make-string val_len 0))
         (set! j 0)
         (%block-void
           (%loop-void
             (%br-if 1 (%i31-ge j val_len))
             (%string-set! val j (%mem-load8 (%i31-add val_ptr j)))
             (set! j (%i31-add j 1))
             (%br 0)))
         (set! result (cons (cons key val) result))
         (set! i (%i31-sub i 1))
         (%br 0)))
     result)))

;; linear-alloc: bump allocator with alignment and memory growth
;; Params: size (i31), align (i31)
;; Returns: aligned pointer (i31)
(define rt-linear-alloc
  '(("size" "align")
    ("aligned" "new_ptr")
    ((set! aligned (%i31-and
                     (%i31-add (%global-get-bump-ptr) (%i31-sub align 1))
                     (%i31-xor (%i31-sub align 1) -1)))
     (set! new_ptr (%i31-add aligned size))
     (%global-set-bump-ptr! new_ptr)
     (if (%i31-gt new_ptr (%memory-size))
         (%memory-grow (%i31-div-u
                         (%i31-add (%i31-sub new_ptr (%memory-size)) 65535)
                         65536))
         0)
     aligned)))

;; bv-alloc: allocate bytevector using linear-alloc
;; Params: size (i31)
;; Returns: bytevector struct (eqref)
(define (make-rt-bv-alloc fn-linear-alloc)
  `(("size")
    ("addr")
    ((set! addr (%call ,fn-linear-alloc size 1))
     (%make-bytevector-struct addr size))))

;; bv-alloc-fill: allocate bytevector and fill with a byte value
;; Params: size (i31), fill (i31)
;; Returns: bytevector struct (eqref)
(define (make-rt-bv-alloc-fill fn-linear-alloc)
  `(("size" "fill")
    ("addr")
    ((set! addr (%call ,fn-linear-alloc size 1))
     (%memory-fill addr fill size)
     (%make-bytevector-struct addr size))))

;; bv-copy-str: copy GC string bytes into bytevector at offset
;; Params: bv (eqref), offset (i31), str (eqref string)
;; Returns: offset + string-length (i31)
(define rt-bv-copy-str
  '(("bv" "offset" "str")
    ("base" "len" "i")
    ((set! base (%bytevector-ptr bv))
     (set! len (%string-length str))
     (set! i 0)
     (%block-void
       (%loop-void
         (%br-if 1 (%i31-ge i len))
         (%mem-store8 (%i31-add (%i31-add base offset) i) (%string-ref str i))
         (set! i (%i31-add i 1))
         (%br 0)))
     (%i31-add offset len))))

;; ptr-to-str: read linear memory bytes into new GC string
;; Params: ptr (i31), len (i31)
;; Returns: GC string (eqref)
(define rt-ptr-to-str
  '(("ptr" "len")
    ("arr" "i")
    ((set! arr (%make-string len 0))
     (set! i 0)
     (%block-void
       (%loop-void
         (%br-if 1 (%i31-ge i len))
         (%string-set! arr i (%mem-load8 (%i31-add ptr i)))
         (set! i (%i31-add i 1))
         (%br 0)))
     arr)))

;; bv-copy-range: copy bytevector sub-range into new bytevector
;; Params: bv (eqref), start (i31), end (i31)
;; Returns: new bytevector (eqref)
(define (make-rt-bv-copy-range fn-linear-alloc)
  `(("bv" "start" "end")
    ("size" "src" "dst")
    ((set! size (%i31-sub end start))
     (set! dst (%call ,fn-linear-alloc size 1))
     (set! src (%i31-add (%bytevector-ptr bv) start))
     (%memory-copy dst src size)
     (%make-bytevector-struct dst size))))

;; bv-copy: copy entire bytevector (delegates to bv-copy-range)
(define (make-rt-bv-copy fn-bv-copy-range)
  `(("bv")
    ()
    ((%call ,fn-bv-copy-range bv 0 (bytevector-length bv)))))

;; bv-copy-from: copy bytevector from start to end (delegates to bv-copy-range)
(define (make-rt-bv-copy-from fn-bv-copy-range)
  `(("bv" "start")
    ()
    ((%call ,fn-bv-copy-range bv start (bytevector-length bv)))))

;; bv-append: concatenate two bytevectors
(define (make-rt-bv-append fn-linear-alloc)
  `(("bv1" "bv2")
    ("len1" "len2" "size" "dst")
    ((set! len1 (bytevector-length bv1))
     (set! len2 (bytevector-length bv2))
     (set! size (%i31-add len1 len2))
     (set! dst (%call ,fn-linear-alloc size 1))
     (%memory-copy dst (%bytevector-ptr bv1) len1)
     (%memory-copy (%i31-add dst len1) (%bytevector-ptr bv2) len2)
     (%make-bytevector-struct dst size))))

;; utf8-to-str-range: convert bytevector sub-range to string
(define (make-rt-utf8-to-str-range fn-ptr-to-str)
  `(("bv" "start" "end")
    ()
    ((%call ,fn-ptr-to-str (%i31-add (%bytevector-ptr bv) start) (%i31-sub end start)))))

;; utf8-to-str: convert entire bytevector to string
(define (make-rt-utf8-to-str fn-utf8-to-str-range)
  `(("bv")
    ()
    ((%call ,fn-utf8-to-str-range bv 0 (bytevector-length bv)))))

;; utf8-to-str-from: convert bytevector from start to end to string
(define (make-rt-utf8-to-str-from fn-utf8-to-str-range)
  `(("bv" "start")
    ()
    ((%call ,fn-utf8-to-str-range bv start (bytevector-length bv)))))

;; str-to-utf8-range: convert string sub-range to bytevector
(define (make-rt-str-to-utf8-range fn-linear-alloc)
  `(("str" "start" "end")
    ("size" "dst" "i")
    ((set! size (%i31-sub end start))
     (set! dst (%call ,fn-linear-alloc size 1))
     (set! i 0)
     (%block-void
       (%loop-void
         (%br-if 1 (%i31-ge i size))
         (%mem-store8 (%i31-add dst i) (%string-ref str (%i31-add start i)))
         (set! i (%i31-add i 1))
         (%br 0)))
     (%make-bytevector-struct dst size))))

;; str-to-utf8: convert entire string to bytevector
(define (make-rt-str-to-utf8 fn-str-to-utf8-range)
  `(("str")
    ()
    ((%call ,fn-str-to-utf8-range str 0 (%string-length str)))))

;; str-to-utf8-from: convert string from start to end to bytevector
(define (make-rt-str-to-utf8-from fn-str-to-utf8-range)
  `(("str" "start")
    ()
    ((%call ,fn-str-to-utf8-range str start (%string-length str)))))


