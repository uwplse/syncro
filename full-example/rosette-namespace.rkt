#lang s-exp rosette

(require rosette/lib/synthax)
(require "types.rkt" "grammar.rkt")

(provide my-for/sum my-for/or run-in-rosette)

;; NOTE: The reimplementations of for can also be found in constructs.rkt
(define-syntax (my-for/sum stx)
  (syntax-case stx ()
    [(_ ([i itr] ...) expr ...)
     (syntax/loc stx
       (let ([sum 0])
         (for ([i itr] ...)
           (set! sum (+ sum (begin expr ...))))
         sum))]))

;; TODO: Not exactly semantically correct -- this will iterate through
;; the entire sequence, even if we could break somewhere in the
;; middle. Should be reimplemented with break. Also change in
;; constructs.rkt.
(define-syntax (my-for/or stx)
  (syntax-case stx ()
    [(_ ([i itr] ...) expr ...)
     (syntax/loc stx
       (let ([val #f])
         (for ([i itr] ...)
           (set! val (or val (begin expr ...))))
         val))]))

(define (run-in-rosette code)
  (eval-syntax (datum->syntax #'3 code)))
