#lang racket

(require (only-in "../../fast_inference/lsl.rkt"
                  [+ +^] [- -^] [* *^] [/ /^]
                  [lift lift^] [def def^]
                  pe))

(provide pe transform)

(define-syntax (transform stx)
  (syntax-case stx (+ - * /)
    [(_ (+ x y))
     (display "Reached +\n")
     (syntax/loc stx (+^ (transform x) (transform y)))]
    [(_ (lift thing))
     (syntax/loc stx (lift^ (transform thing)))]
    [(_ (def thing))
     (syntax/loc stx (def^ (transform thing)))]
    [(_ (expr ...))
     (syntax/loc stx ((transform expr) ...))]
    [(_ thing)
     (syntax/loc stx thing)]))
