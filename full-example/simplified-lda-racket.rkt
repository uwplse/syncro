#lang racket

(provide (all-defined-out))

;(require "simplified-lda-rosette.rkt")
;(require "simplified-lda-lsl.rkt")

(require "rosette-namespace.rkt")
(require "lsl-namespace.rkt")
(require "lsl-transformer.rkt")

(define x 4)
(define y 6)

(define z
  (eval `(begin
           (define (get-middle a b)
             (define-symbolic x integer?)
             (define model
               (synthesize #:forall '()
                           #:guarantee (begin (assert (> x a))
                                              (assert (< x b)))))
             (evaluate x model))
           (get-middle ,x ,y))
        rosette-ns))

(print z)
(newline)

;(add3 x y z)

;(define fn (padd3 #'x y z))

;(define (qadd3 x y z)
;  (+ x (+ y z)))

(define fn2 (pe (thunk (transform (+ #'x (+ y z))))))
