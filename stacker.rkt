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
     HANDLE-EXPR ...
     (display (first stack))))

(provide (rename-out [stacker-module-begin #%module-begin]))

(define stack empty)

(define (pop-stack!)
  (let ([arg (first stack)])
    (set! stack (rest stack))
    arg))

(define (push-stack! arg)
  (set! stack (cons arg stack)))

(define (handle [arg #f])
  (cond
    [(number? arg) (push-stack! arg)]
    [(or (equal? + arg) (equal? * arg))
     (let ([op-result (arg (pop-stack!) (pop-stack!))])
       (push-stack! op-result))]
    ))
(provide handle)

(provide + *)