#lang racket

(require (only-in "../../fast_inference/lsl.rkt"
                  [+ +^] [- -^] [* *^] [/ /^]
                  pe))

(provide pe transform)

(define-syntax (transform stx)
  (syntax-case stx (+ - * /)
    [(_ (+ x y))
     (syntax/loc stx (+^ x y))]
    [(_ (+ x y))
     (syntax/loc stx (+^ x y))]
    [(_ (+ x y))
     (syntax/loc stx (+^ x y))]
    [(_ (+ x y))
     (syntax/loc stx (+^ x y))]
    [(_ (expr ...))
     (syntax/loc stx ((transform expr) ...))]
    [(_ thing)
     (syntax/loc stx thing)]))
