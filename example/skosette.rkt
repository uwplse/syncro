#lang incremental

;; An example from Skosette.

(define-structure r1 #:type (Vector-type 2 (List-type 3 (Boolean-type)))
  #:initialize (make-vector 2 '())
  #:deltas
  [(define (add-to-r1! [i (Integer-type)] [formula (Boolean-type)])
     (vector-set! r1 i (cons formula (vector-ref r1 i))))])

(define-structure ψ  #:type (Vector-type 2 (Boolean-type))
  ;; We want a disjunction on the lst, as in (for/or ([x lst]) x)
  ;; However, since lst is symbolic, we can't use loops, so instead we
  ;; use member #t, which is semantically equivalent
  #:value (vector-map (lambda (lst) (not (member #t lst))) r1)
  #:depends (r1))
