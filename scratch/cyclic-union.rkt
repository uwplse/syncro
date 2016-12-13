#lang rosette

(define-symbolic b b2 b3 boolean?)

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

(displayln "Calling thing")
(println vec-to-set)
(symbolics vec-to-set)
(displayln "Call successful")
