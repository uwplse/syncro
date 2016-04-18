#lang s-exp rosette

(require rosette/lib/synthax)

(provide get-middle)

(define (get-middle a b)
  (define-symbolic x integer?)
  (define model
    (synthesize #:forall '()
                #:guarantee (begin (assert (> x a))
                                   (assert (< x b)))))
  (evaluate x model))
