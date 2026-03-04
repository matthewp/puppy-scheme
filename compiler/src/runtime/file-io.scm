;;; file-io.scm — File I/O runtime functions

;; open-input-file: use get-directories + open-at + read-via-stream
;; Stores input-stream handle in fd, descriptor handle in pos
(define rt-open-input-file
  '(("path")
    ("len" "i" "start" "plen" "ptr" "count" "dirhandle" "disc")
    ((set! len (%string-length path))
     (if (if (%i31-gt len 0) (%i31-eqz (%i31-sub (%string-ref path 0) 47)) #f)
         (begin (set! start 1) (set! plen (%i31-sub len 1)))
         (begin (set! start 0) (set! plen len)))
     (set! i 0)
     (%block-void
       (%loop-void
         (%br-if 1 (%i31-ge i plen))
         (%mem-store8 (%i31-add 1024 i) (%string-ref path (%i31-add start i)))
         (set! i (%i31-add i 1))
         (%br 0)))
     (%call-get-directories 0)
     (set! ptr (%mem-load32 0))
     (set! count (%mem-load32 4))
     (set! i 0)
     (set! disc 1)
     (%block-void
       (%loop-void
         (%br-if 1 (%i31-ge i count))
         (set! dirhandle (%mem-load32 (%i31-add ptr (%i31-mul i 12))))
         (%call-open-at dirhandle 0 1024 plen 0 1 8)
         (set! disc (%mem-load32 8))
         (%br-if 1 (%i31-eqz disc))
         (set! i (%i31-add i 1))
         (%br 0)))
     (if (%i31-eqz disc)
         (begin
           (%call-read-via-stream (%mem-load32 12) 0 16)
           (if (%i31-eqz (%mem-load32 16))
               (%make-port (%mem-load32 20) 0 -1 (%ref-null) (%mem-load32 12))
               (%unreachable)))
         (%unreachable)))))

;; open-output-file: use get-directories + open-at + write-via-stream
;; open-flags: 1=create|8=truncate=9, desc-flags: 2=write
(define rt-open-output-file
  '(("path")
    ("len" "i" "start" "plen" "ptr" "count" "dirhandle" "disc")
    ((set! len (%string-length path))
     (if (if (%i31-gt len 0) (%i31-eqz (%i31-sub (%string-ref path 0) 47)) #f)
         (begin (set! start 1) (set! plen (%i31-sub len 1)))
         (begin (set! start 0) (set! plen len)))
     (set! i 0)
     (%block-void
       (%loop-void
         (%br-if 1 (%i31-ge i plen))
         (%mem-store8 (%i31-add 1024 i) (%string-ref path (%i31-add start i)))
         (set! i (%i31-add i 1))
         (%br 0)))
     (%call-get-directories 0)
     (set! ptr (%mem-load32 0))
     (set! count (%mem-load32 4))
     (set! i 0)
     (set! disc 1)
     (%block-void
       (%loop-void
         (%br-if 1 (%i31-ge i count))
         (set! dirhandle (%mem-load32 (%i31-add ptr (%i31-mul i 12))))
         (%call-open-at dirhandle 0 1024 plen 9 2 8)
         (set! disc (%mem-load32 8))
         (%br-if 1 (%i31-eqz disc))
         (set! i (%i31-add i 1))
         (%br 0)))
     (if (%i31-eqz disc)
         (begin
           (%call-write-via-stream (%mem-load32 12) 0 16)
           (if (%i31-eqz (%mem-load32 16))
               (%make-port (%mem-load32 20) 1 -1 (%ref-null) (%mem-load32 12))
               (%unreachable)))
         (%unreachable)))))

;; close-port: drop stream + descriptor handles
(define rt-close-port
  '(("port")
    ()
    ((if (%i31-ge (%port-fd port) 0)
         (begin
           (if (%i31-eqz (%port-mode port))
               (%call-drop-input-stream (%port-fd port))
               (%call-drop-output-stream (%port-fd port)))
           (if (%i31-ge (%port-pos port) 0)
               (%call-drop-descriptor (%port-pos port))
               0))
         0)
     0)))

;; read-char: check peek buf, then string port or stream-read
;; Stream-read result at retptr=300: disc@300, ptr@304, len@308
(define rt-read-char
  '(("port")
    ("buf" "pos" "str" "dptr" "dlen")
    ((set! buf (%port-buf port))
     (if (%i31-ge buf 0)
         (begin
           (%port-set-buf! port -1)
           (%make-char buf))
         (if (%ref-is-null (%port-str port))
             (begin
               (%call-stream-read (%port-fd port) 1 300)
               (if (%i31-eqz (%mem-load32 300))
                   (begin
                     (set! dlen (%mem-load32 308))
                     (if (%i31-eqz dlen)
                         (%eof-new)
                         (%make-char (%mem-load8 (%mem-load32 304)))))
                   (%eof-new)))
             (begin
               (set! pos (%port-pos port))
               (set! str (%port-str port))
               (if (%i31-ge pos (%string-length str))
                   (%eof-new)
                   (begin
                     (%port-set-pos! port (%i31-add pos 1))
                     (%make-char (%string-ref str pos))))))))))

;; peek-char: like read-char but doesn't advance
(define rt-peek-char
  '(("port")
    ("buf" "ch" "pos" "str" "dlen")
    ((set! buf (%port-buf port))
     (if (%i31-ge buf 0)
         (%make-char buf)
         (if (%ref-is-null (%port-str port))
             (begin
               (%call-stream-read (%port-fd port) 1 300)
               (if (%i31-eqz (%mem-load32 300))
                   (begin
                     (set! dlen (%mem-load32 308))
                     (if (%i31-eqz dlen)
                         (%eof-new)
                         (begin
                           (set! ch (%mem-load8 (%mem-load32 304)))
                           (%port-set-buf! port ch)
                           (%make-char ch))))
                   (%eof-new)))
             (begin
               (set! pos (%port-pos port))
               (set! str (%port-str port))
               (if (%i31-ge pos (%string-length str))
                   (%eof-new)
                   (%make-char (%string-ref str pos)))))))))

;; write-char: use stream-write on port's output-stream handle
(define rt-write-char
  '(("ch" "port")
    ()
    ((%mem-store8 24 (%char-code ch))
     (%stream-write (%port-fd port) 24 1 300)
     0)))

;; open-input-string: (str) → port
;; Create a string port with fd=-1
(define rt-open-input-string
  '(("str")
    ()
    ((%make-port -1 0 -1 str 0))))
