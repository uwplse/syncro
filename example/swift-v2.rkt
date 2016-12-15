#lang racket

;; An optimization in the Swift compiler -- identifying and repairing
;; Markov blankets of affected variables when the graph structure
;; changes.
;; Unlike swift.rkt, here we assume that we do not have access to a
;; Graph type, and so we have to model it ourselves.

(require "../src/racket/constructs.rkt")

;;;;;;;;;;;;;;;
;; Constants ;;
;;;;;;;;;;;;;;;

(define-constant NUM_NODES (Integer-type) 10)
(define-enum-type Node NUM_NODES)

;;;;;;;;;;;;;;;;;;;;
;; Mutable values ;;
;;;;;;;;;;;;;;;;;;;;

;; Asserts that the graph defined by the adjacency list is acyclic.
;; In particular, since the ordering on nodes does not matter, we will
;; require that the nodes already be topologically sorted, that is, if
;; j > i, then there is no edge from j --> i.
(define (assert-acyclic children)
  (for ([node NUM_NODES])
    
    (define ch (vector-ref children node))
    ;; No self-edges
    (assert (not (set-member? node ch)))

    ;; No back-edges
    (for ([prior-node node])
      (assert (not (set-member? prior-node ch))))))
      

;; An update that allows us to replace one child with another, or one
;; parent with another.
(define-update (replace struc [node Node] [old-value Node] [new-value Node])
  (let ([s (vector-ref struc node)])
    (set-remove! s old-value)
    (set-add! s new-value)))

(define-incremental node->children (Vector-type Node (Set-type Node)) () (replace)
  #:assume (assert-acyclic node->children))

(define-incremental node->parents (Vector-type Node (Set-type Node)) () (replace)
  (let ([vec (build-vector NUM_NODES
                           (lambda (i) (make-set NUM_NODES)))])
    (for* ([node NUM_NODES]
           [n (vector-ref node->children node)])
      (set-add! (vector-ref vec n) node))
    vec))

  ;; Alternative functional style:
  ;; (build-vector
  ;;  NUM_NODES
  ;;  (lambda (node)
  ;;    (list->set
  ;;     (filter (lambda (n) (set-member? node (vector-ref node->parents n)))
  ;;             (range NUM_NODES)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Incremental structures ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-incremental mb (Vector-type Node (Set-type Node)) (graph) ()
  (build-vector
   NUM_NODES
   (lambda (node)
     ;; Markov blanket consists of parents, children, and children's
     ;; parents (aunts)
     (let* ([parents (vector-ref node->parents node)]
            [children (vector-ref node->children node)]
            [aunts (apply set-union
                          (map (curry vector-ref node->parents)
                               (set->list children)))])

       ;; Don't include the current node
       (set-remove (set-union parents children aunts) node)))))

       ;; Alternatively, a set theoretic definition:
       ;; parents + children +
       ;; { n | n != node ∧ ∃ m : m ∈ children[n] ∧ m ∈ children[node] }
       ;; (set-union
       ;;  parents children
       ;;  (make-set
       ;;   NUM_NODES
       ;;   (lambda (n)
       ;;     (define other-children (node-children graph n))
       ;;     (and (not (equal? n node))
       ;;          (for/or ([m (in-range NUM_NODES)])
       ;;            (and (set-member? m children)
       ;;                 (set-member? m other-children)))))))

;;;;;;;;;;;;;;;
;; Algorithm ;;
;;;;;;;;;;;;;;;

(define (main)
  ;; TODO: Simple example of a contingent Bayes net which we then
  ;; run Gibbs sampling on
  (void))
