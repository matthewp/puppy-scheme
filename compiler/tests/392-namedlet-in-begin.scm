;; Test: named-let with set! on captured var - loop must iterate fully
;; Reproduces the pattern from codegen-expr begin handler
;; expect: 5

(let ((ok #t)
      (result 0))
  (let loop ((xs '(a b c d e)) (n 0))
    (when (and ok (pair? xs))
      (set! ok #t)
      (set! result (+ n 1))
      (loop (cdr xs) (+ n 1))))
  (display result)
  (newline))
