;; expect: 10
;; Test: let* with (set! var ...) inside (when ...) — the pattern used
;; in codegen.scm for computing type counts.
;; offset starts at 8, gets incremented conditionally.
(let* ((needs-a #t)
       (needs-b #f)
       (needs-c #t)
       (offset 8)
       (_ (when needs-a (set! offset (+ offset 1))))
       (_ (when needs-b (set! offset (+ offset 1))))
       (_ (when needs-c (set! offset (+ offset 1))))
       (result offset))
  (display result)
  (newline))
