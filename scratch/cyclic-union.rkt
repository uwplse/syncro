#lang rosette

(define-symbolic b b2 b3 b4 boolean?)

(define vec-of-vecs
  (vector (vector 1 2) (vector 3 4) (vector 5 6)))

(define vec-to-set
  (if b vec-of-vecs (vector-ref vec-of-vecs 0)))

(define val-to-set
  (if b2
      (if b (vector-ref vec-of-vecs 0) 8)
      (if b (vector-ref vec-of-vecs 0) 8)))

;; This creates a cyclic union because Rosette considers the case
;; where it tries (vector-set! vec0 0 vec0)
;; where vec0 is (vector-ref vec-of-vecs 0)
(vector-set! vec-to-set 0 val-to-set)

;; Used to print infinitely, now fixed:
;; (println vec-to-set)
;; Used to result in an infinite loop, now fixed:
;; (symbolics vec-to-set)

;; Repeat all the same stuff with new vectors and symbolic variables
(define new-vec-of-vecs
  (vector (vector 1 2) (vector 3 4) (vector 5 6)))

(define new-vec-to-set
  (if b3 new-vec-of-vecs (vector-ref new-vec-of-vecs 0)))

(define new-val-to-set
  (if b4
      (if b3 (vector-ref new-vec-of-vecs 0) 8)
      (if b3 (vector-ref new-vec-of-vecs 0) 8)))

(vector-set! new-vec-to-set 0 new-val-to-set)

(displayln "Calling equal?")
(equal? vec-to-set new-vec-to-set)
(displayln "Call successful")
