;;; file-exists.scm — file-exists? runtime function

;; file-exists?: use get-directories + open-at, drop descriptor if opened
(define rt-file-exists
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
           (%call-drop-descriptor (%mem-load32 12))
           #t)
         #f))))
