#lang s-exp "../../fast_inference/lsl.rkt"

(provide lift add3 padd3 pe transform)

(define (add3 x y z)
  (+ x (+ y z)))

(define (padd3 x y z)
  (pe (thunk (add3 x y z))))

;; parameterize -- how to apply to environments?

;; Keep a list of all procedures that we can switch between in Racket/Rosette/LSL, and then write transformer functions that switch between them
