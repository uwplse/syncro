#lang racket

;; An optimization in the Swift compiler -- identifying and repairing
;; Markov blankets of affected variables when the graph structure
;; changes.

(require "../src/racket/constructs.rkt")

;;;;;;;;;;;;;;;
;; Constants ;;
;;;;;;;;;;;;;;;

(define-constant NUM_NODES (Integer-type) 10)
(define-enum-type Node NUM_NODES)

;;;;;;;;;;;;;;;;;;;;
;; Mutable values ;;
;;;;;;;;;;;;;;;;;;;;

;; TODO: Typically define-incremental requires an initialization
;; expression, but that doesn't make sense for graph.
;; This also arises when defining symbolic constants, such as
;; word->document in LDA.
;; Need to have a mechanism to use symbolic values at compile time and
;; replace them with concrete initial values at run time.
;; TODO: Ideally, the programmer would be able to say "the only update
;; I care about is remove-child followed by add-child on the same
;; node", so something like:
;; (define-update (change-child node old-child new-child)
;;   (remove-child-graph! node old-child)
;;   (add-child-graph! node new-child))
;; However, then we must somehow infer the various properties of
;; updates that are used in synthesis. (See symbolic-update-code in
;; types.rkt -- we need to be able to create that function
;; automatically for the new update.)
(define-incremental graph (DAG-type Node NUM_NODES) () (add-child remove-child))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Incremental structures ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-incremental mb (Vector-type Node (Set-type Node)) (graph) ()
  (build-vector
   NUM_NODES
   (lambda (node)
     ;; Markov blanket consists of parents, children, and children's
     ;; parents (aunts)
     (let* ([parents (node-parents graph node)]
            [children (node-children graph node)]
            [aunts (apply set-union
                          (map (curry node-parents graph)
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
