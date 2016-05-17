#lang s-exp rosette

(require "../example/synthesize-util.rkt" "../example/util.rkt")

(provide rosette-ns my-for/sum my-for/or)

;; NOTE: The reimplementations of for can also be found in constructs.rkt
(define-syntax (my-for/sum stx)
  (syntax-case stx ()
    [(_ ([i itr] ...) expr ...)
     (syntax/loc stx
       (let ([sum 0])
         (for ([i itr] ...)
           (set! sum (+ sum (begin expr ...))))
         sum))]))

;; TODO: Reimplement with break? If so, also change in constructs.rkt
(define-syntax (my-for/or stx)
  (syntax-case stx ()
    [(_ ([i itr] ...) expr ...)
     (syntax/loc stx
       (let ([val #f])
         (for ([i itr] ...)
           (set! val (or val (begin expr ...))))
         val))]))

(define-namespace-anchor anchor)
(define rosette-ns (namespace-anchor->namespace anchor))
