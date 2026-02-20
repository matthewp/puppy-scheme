;; expect: 20
;; Test: let binding with complex init expressions that expand to nested
;; lets (e.g., append/length desugaring). The nested let's temporaries
;; must not overwrite earlier let bindings' locals.
(let ((buf (vector 10 20 30))
      (names (append '(a) '(b c)))
      (n (length '(x y z))))
  (display (vector-ref buf 1))
  (newline))
