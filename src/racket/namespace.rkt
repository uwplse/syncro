#lang racket

(require "../../../synapse/opsyn/engine/search.rkt")

(provide run-in-racket) ; racket-ns)

;(define-namespace-anchor anchor)
;(define racket-ns (namespace-anchor->namespace anchor))

(define (run-in-racket code)
  (eval (datum->syntax #'3 code)))
