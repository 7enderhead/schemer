#lang br/quicklang
(define (read-syntax path port)
  ; read all source lines
  (define src-lines (port->lines port))
  ; wrap each line's content in (handle ...)
  (define src-datums (format-datums '(handle ~a) src-lines))
  ; insert each such datum into the module we are returning via quasiquote
  (define module-datum `(module stacker-mod "stacker.rkt" ,@src-datums))
  ; return the module definition as a syntax object
  (datum->syntax #f module-datum))
(provide read-syntax)

(define-macro (stacker-module-begin HANDLE-EXPR ...)
  #'(#%module-begin
     HANDLE-EXPR ...))
(provide (rename-out [stacker-module-begin #%module-begin]))