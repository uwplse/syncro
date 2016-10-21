#lang s-exp "../../fast_inference/lsl.rkt"

(provide lsl-ns)

(define-namespace-anchor anchor)
(define lsl-ns (namespace-anchor->namespace anchor))
