#lang rosette

(require "util.rkt")

(provide vector-sum vector-increment! vector-decrement!
         (rename-out [general-vector-ref vector-ref]
                     [general-vector-set! vector-set!]))

(define (vector-sum vec)
  (my-for/sum ([x vec]) x))

;; Vector increment and decrement are meant for vectors of integers,
;; so they won't use the general vector operations that work on
;; bitvectors.
(define (vector-increment! vec index)
  (vector-set! vec index
               (+ (vector-ref vec index) 1)))

(define (vector-decrement! vec index)
  (vector-set! vec index
               (- (vector-ref vec index) 1)))

(define (general-vector-ref vec idx)
  (define int-idx
    (if (bv? idx) (bitvector->integer idx) idx))
  (vector-ref vec int-idx))

(define (general-vector-set! vec idx val)
  (define int-idx
    (if (bv? idx) (bitvector->integer idx) idx))
  (vector-set! vec int-idx val))
