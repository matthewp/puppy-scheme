;; expect: #tbar
(display (symbol? (string->symbol "foo")))
(display (symbol->string (string->symbol "bar")))
