#lang s-exp rosette

(provide rosette-ns)

(define-namespace-anchor anchor)
(define rosette-ns (namespace-anchor->namespace anchor))
