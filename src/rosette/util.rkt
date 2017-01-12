#lang rosette

(provide internal-error
         input-val input-preconditions input?
         (rename-out [input make-input]))

(define (internal-error str)
  (displayln str)
  (error str))

(struct input (val preconditions) #:transparent)
