(define-library (puppykit ui)
  (export
    ;; Component system
    define-component
    create-instance render-instance dispatch-instance
    ;; WIT ABI exports
    create render dispatch alloc
    ;; HTML macro
    html
    ;; Rendering
    render-to-string
    ;; Serialization
    serialize-opcodes)
  (include "../html.scm")
  (include "../component.scm")
  (include "../opcodes.scm"))
