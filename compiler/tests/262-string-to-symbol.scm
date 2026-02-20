;; expect: 1bar
(display (symbol? (string->symbol "foo")))
(display (symbol->string (string->symbol "bar")))
