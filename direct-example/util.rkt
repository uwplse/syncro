#lang s-exp rosette

(require "enum-set.rkt")

(provide vector-sum vector-increment! vector-decrement!
         matrix-get matrix-increment! matrix-decrement!
         add-in-vector! remove-in-vector! contained-in-vector?)

;;;;;;;;;;;;;;;;;;;;;;;;
;; simplified-lda.rkt ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define (vector-sum vec)
  (define result 0)
  (for ([v vec])
    (set! result (+ result v)))
  result)

(define (vector-increment! vec index)
  (vector-set! vec index
               (+ (vector-ref vec index) 1)))

(define (vector-decrement! vec index)
  (vector-set! vec index
               (- (vector-ref vec index) 1)))

;;;;;;;;;;;;;;;
;; swift.rkt ;;
;;;;;;;;;;;;;;;

(define (matrix-get matrix x y)
  (vector-ref (vector-ref matrix x) y))

(define (matrix-increment! matrix x y)
  (define vec (vector-ref matrix x))
  (vector-set! vec y (+ (vector-ref vec y) 1)))

(define (matrix-decrement! matrix x y)
  (define vec (vector-ref matrix x))
  (vector-set! vec y (- (vector-ref vec y) 1)))

(define (add-in-vector! vec index element)
  (enum-set-add! (vector-ref vec index) element))

(define (remove-in-vector! vec index element)
  (enum-set-remove! (vector-ref vec index) element))

(define (contained-in-vector? vec index element)
  (enum-set-contains? (vector-ref vec index) element))
