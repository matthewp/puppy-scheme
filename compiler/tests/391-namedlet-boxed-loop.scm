;; Test: named-let loop inside let with set!+captured variable
;; The loop variable is boxed (set! + self-reference), verify recursion works
;; expect: 12345

(let ((ok #t))
  (let loop ((xs '(1 2 3 4 5)) (i 0))
    (when (and ok (pair? xs))
      (set! ok (begin (display (car xs)) #t))
      (loop (cdr xs) (+ i 1)))))
(newline)
