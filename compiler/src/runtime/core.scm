;;; core.scm — Core runtime functions

(define rt-make-complex
  '(("real" "imag")   ;; params
    ()                 ;; locals
    ((if (%i31? imag)
         (if (%i31-eqz imag)
             real
             (%make-complex-raw real imag))
         (%make-complex-raw real imag)))))
