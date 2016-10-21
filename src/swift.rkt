#lang racket

;; TODO: Write the enum set types and data structures
(require "constructs.rkt")

(define-constant NUM_NODES (Integer-type) 10)

(define-enum-type Node NUM_NODES)

(define (assert-acyclic children)
  ;; TODO: Implement
  (void))

(define-mutable children (Vector-type Node (Enum-Set-type Node))
  ()
  (add remove)
  (build-vector NUM_NODES (lambda (node) (enum-set '())))
  #:assert
  (begin
    (assert-acyclic children)

    ;; These assertions are implied by assert-acyclic, but may help the SAT solver
    (for ([node NUM_NODES])
      (assert (not (enum-set-member node (vector-ref children node))))
      ;; TODO: This for loop probably will not work since it is
      ;; iterating over something symbolic
      (for ([child (vector-ref children node)])
        (assert (not (enum-set-member node (vector-ref children child))))))))

(define-incremental parents (Vector-type Node (Enum-Set-type Node))
  (children)
  (add remove)
  (build-vector
   NUM_NODES
   (lambda (n1)
     (enum-set
      (filter (lambda (n2) (enum-set-member n1 (vector-ref children n2)))
              (range NUM_NODES))))))

(define-incremental mb (Vector-type Node (Enum-Set-type Node))
  (children parents)
  (add remove)
  (build-vector
   NUM_NODES
   (lambda (n1)
     (enum-set
      (filter
       (lambda (n2)
         (and (not (equal? n1 n2))
              (or (enum-set-member n2 (vector-ref children n1))
                  (enum-set-member n2 (vector-ref parents n1))
                  ;; TODO: Won't work, iterating over something symbolic
                  (my-for/or ([child (vector-ref children n1)])
                             (enum-set-member n2 (vector-ref parents
                                                             child))))))
       (range NUM_NODES))))))

(finalize)

;; TODO: Hurricane model, perhaps?
