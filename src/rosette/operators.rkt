#lang rosette

(require "util.rkt")

(provide vector-sum vector-increment! vector-decrement!)

(define (vector-sum vec)
  (my-for/sum ([x vec]) x))

(define (vector-increment! vec index)
  (vector-set! vec index
               (+ (vector-ref vec index) 1)))

(define (vector-decrement! vec index)
  (vector-set! vec index
               (- (vector-ref vec index) 1)))
