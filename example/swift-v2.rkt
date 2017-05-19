#lang incremental

;; An optimization in the Swift compiler -- identifying and repairing
;; Markov blankets of affected variables when the graph structure
;; changes.
;; Unlike swift.rkt, here we assume that we do not have access to a
;; Graph type, and so we have to model it ourselves.

;;;;;;;;;;;;;;;
;; Constants ;;
;;;;;;;;;;;;;;;

(define-symbolic NUM_NODES #:type (Integer-type) #:configs [10])
(define-enum-type Node NUM_NODES)

;;;;;;;;;;;;;;;;;;;;
;; Mutable values ;;
;;;;;;;;;;;;;;;;;;;;

(define-incremental node->children #:type (Vector-type Node (Set-type Node))
  ;; Asserts that the graph defined by the adjacency list is acyclic.
  ;; In particular, since the ordering on nodes does not matter, we
  ;; will require that the nodes already be topologically sorted, that
  ;; is, if j > i, then there is no edge from j --> i.
  #:invariant
  (for ([node NUM_NODES])
    (define ch (vector-ref node->children node))
    ;; No self-edges
    (assert (not (enum-set-contains? ch node)))
    ;; No back-edges
    (for ([prior-node node])
      (assert (not (enum-set-contains? ch prior-node)))))
  #:initialize (void)
  #:updates
  ;; An update that allows us to replace one child with another.
  [(define (replace [node Node] [old-value Node] [new-value Node])
     (let ([c (vector-ref node->children node)])
       (enum-set-remove! c old-value)
       (enum-set-add! c new-value)))])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Incremental structures ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-incremental node->parents #:type (Vector-type Node (Set-type Node))
  #:value
  (let ([vec (build-vector NUM_NODES
                           (lambda (i) (enum-make-set NUM_NODES)))])
    (for ([node NUM_NODES])
      (for-enum-set ([n (vector-ref node->children node)])
        (enum-set-add! (vector-ref vec n) node)))
    vec)
  #:depends (node->children))

  ;; Alternative functional style:
  ;; (build-vector
  ;;  NUM_NODES
  ;;  (lambda (node)
  ;;    (list->set
  ;;     (filter (lambda (n) (enum-set-contains? (vector-ref node->parents n) node))
  ;;             (range NUM_NODES)))))

(define-incremental mb #:type (Vector-type Node (Set-type Node))
  #:value
  (build-vector
   NUM_NODES
   (lambda (node)
     ;; Markov blanket consists of parents, children, and children's
     ;; parents (aunts)
     (let* ([parents (vector-ref node->parents node)]
            [children (vector-ref node->children node)]
            [aunts (apply enum-set-union
                          (map (curry vector-ref node->parents)
                               (enum-set->list children)))])

       (define result (enum-set-union parents children aunts))
       ;; Don't include the current node
       (enum-set-remove! result node)
       result)))
  #:depends (node->children))

       ;; Alternatively, a set theoretic definition:
       ;; parents + children +
       ;; { n | n != node ∧ ∃ m : m ∈ children[n] ∧ m ∈ children[node] }
       ;; (enum-set-union
       ;;  parents children
       ;;  (build-enum-set
       ;;   NUM_NODES
       ;;   (lambda (n)
       ;;     (define other-children (node-children graph n))
       ;;     (and (not (equal? n node))
       ;;          (for/or ([m (in-range NUM_NODES)])
       ;;            (and (enum-set-contains? children m)
       ;;                 (enum-set-contains? other-children m)))))))

;;;;;;;;;;;;;;;
;; Algorithm ;;
;;;;;;;;;;;;;;;

(algorithm
 (define (main)
   ;; TODO: Simple example of a contingent Bayes net which we then
   ;; run Gibbs sampling on
   (void)))
