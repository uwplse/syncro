#lang rosette

(provide internal-error)

(define (internal-error str)
  (displayln str)
  (error str))
