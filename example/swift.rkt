#lang incremental

;; An optimization in the Swift compiler -- identifying and repairing
;; Markov blankets of affected variables when the graph structure
;; changes.

;;;;;;;;;;;;;;;
;; Constants ;;
;;;;;;;;;;;;;;;

(define NUM_NODES (Integer-type) 5)
(define-enum-type Node NUM_NODES)

;;;;;;;;;;;;;;;;;;;;
;; Mutable values ;;
;;;;;;;;;;;;;;;;;;;;

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
(define-incremental graph (DAG-type Node)
  #:initialize (make-graph NUM_NODES)
  #:updates
  [(define (add-child! [parent Node] [child Node])
     (add-edge! graph parent child))
   (define (remove-child! [parent Node] [child Node])
     (remove-edge! graph parent child))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Incremental structures ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-incremental mb (Vector-type Node (Set-type Node))
  #:value
  (build-vector
   NUM_NODES
   (lambda (node)
     ;; Markov blanket consists of parents, children, and children's
     ;; parents (aunts)
     (let* ([parents (vertex-parents graph node)]
            [children (vertex-children graph node)]
            [children-list (enum-set->list children)]
            [aunts (if (null? children-list)
                       (enum-make-set NUM_NODES)
                       (apply enum-set-union
                              (map (curry vertex-parents graph)
                                   children-list)))]
            [result (enum-set-union parents children aunts)])
       ;; Don't include the current node
       (enum-set-remove! result node)
       result)))
  #:depends (graph)
  #:sketches
  [(add-child!
    (lambda (parent child)
      ;; Fix things between parent and child
      (??)
      ;; Fix things for new aunt relations
      (for-enum-set ([other-parent (vertex-parents graph child)])
        (??))))
   (remove-child!
    (lambda (parent child)
      (??)
      (for-enum-set ([other-parent (vertex-parents graph child)])
        (??))))])

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

(algorithm
 (define (main)
   ;; TODO: Simple example of a contingent Bayes net which we then
   ;; run Gibbs sampling on
   (void)))
