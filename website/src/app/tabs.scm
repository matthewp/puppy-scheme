(define code-snippet
  (string-append
    "(define-component counter\n"
    "  (state (count 0))\n"
    "  (render\n"
    "    (html (div (@ (class \"counter\"))\n"
    "            (button (@ (on \"click\" \"decrement\"))\n"
    "              \"-\")\n"
    "            (span (@ (class \"count\"))\n"
    "              ,(number->string count))\n"
    "            (button (@ (on \"click\" \"increment\"))\n"
    "              \"+\"))))\n"
    "  (dispatch event\n"
    "    (cond ((equal? event \"decrement\")\n"
    "           (if (> count 0)\n"
    "               (set! count (- count 1))))\n"
    "          ((equal? event \"increment\")\n"
    "           (set! count (+ count 1))))))\n"))

(define-component tabs
  (state (active 0))
  (render
    (html (div (@ (class "tabs"))
            (div (@ (class "tab-bar"))
              (button (@ (class ,(if (= active 0) "tab active" "tab"))
                        (on "click" "tab-0"))
                "Components")
              (button (@ (class ,(if (= active 1) "tab active" "tab"))
                        (on "click" "tab-1"))
                "CLIs"))
            (div (@ (class "tab-panel") (style ,(if (= active 0) "" "display:none")))
              (h3 "Components")
              (p "Puppy Scheme components use closures for state. The "
                 (code "define-component") " macro expands to a factory that "
                 "creates fresh bindings per instance.")
              (child counter)
              (pre (code (@ (class "language-scheme")) ,code-snippet)))
            (div (@ (class "tab-panel") (style ,(if (= active 1) "" "display:none")))
              (child cli-example)))))
  (dispatch event
    (cond ((equal? event "tab-0") (set! active 0))
          ((equal? event "tab-1") (set! active 1)))))
