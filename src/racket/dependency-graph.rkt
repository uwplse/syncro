#lang racket

(require graph)

(require "../rosette/types.rkt")

(provide make-dependency-graph node%
         add-node! add-dependency!
         get-node get-ids)

;;;;;;;;;;;;;;;;;;;;;;
;; Dependency graph ;;
;;;;;;;;;;;;;;;;;;;;;;

;; A dependency graph consists of a graph, a mapping from vertices of
;; the graph to nodes containing more information, and a list of
;; variables in the order they were added to the graph.
;; The graph can give us a list of variables but does not guarantee order.
(struct dep-graph (graph id->node id->node-set! [id-list #:mutable]))

;; Creates a new, empty dep-graph
(define (make-dependency-graph)
  (let ([g (directed-graph '())])
    (define-vertex-property g id->node)
    (dep-graph g id->node id->node-set! '())))

;; dg:   dep-graph
;; id:   symbol, name of the variable to be added
;; node: node%, auxiliary information about the variable
;; Adds the node to the dependency graph.
(define (add-node! dg id node)
  (match-let ([(dep-graph graph id->node id->node-set! id-list) dg])
    (when (has-vertex? graph id)
      (error (format "Symbol has already been used: ~a" id)))
    (add-vertex! graph id)
    (id->node-set! id node)
    (set-dep-graph-id-list! dg (append id-list (list id)))))

;; dg: dep-graph
;; parent, child: symbols, names of variable to create a dependency for
(define (add-dependency! dg parent child)
  (add-directed-edge! (dep-graph-graph dg) parent child))

;; dg: dep-graph
;; id: symbol (variable name)
(define (get-node dg id)
  ((dep-graph-id->node dg) id))

;; dg: dep-graph
;; Returns the names of all variables added to the dependency graph.
(define (get-ids dg)
  (dep-graph-id-list dg))

;;;;;;;;;;;
;; Nodes ;;
;;;;;;;;;;;

(define node%
  (class* object% (writable<%>)
    (super-new)

    ;; id:           Name of the variable
    ;; type:         Type of the variable
    ;; update-types: The kinds of updates allowed
    ;; fn-code:      Expression to recompute the value of the node
    (init-field id type update-types fn-code)

    ;; A map that specifies, for each update type, the names of the
    ;; arguments to the generated mutator for that update-type
    ;; For example, for update type 'assign to a vector v, we might
    ;; add the mapping 'assign -> '(index1 val2)
    ;; Then the update procedure should look like
    ;; (define (assign-v! index1 val2) ...)
    (init-field [update-arg-names (make-hash)])

    ;; Maps each update type to its incremental update function
    ;; (represented as an S-expression as a nested list).
    (define update-fns (make-hash))

    (define/public (custom-write port)
      (display (format "#<structure%:~a>" id) port))

    (define/public (custom-display port)
      (display (format "#<structure%:~a>" id) port))

    (define/public (get-id) id)
    (define/public (get-type) type)
    (define/public (get-update-types) update-types)
    (define/public (get-fn-code) fn-code)

    ;; Gets update arg names (see above). If no arg names exist yet,
    ;; we generate them by consulting the type.
    (define/public (get-update-arg-names update-type)
      (if (hash-has-key? update-arg-names update-type)
          (hash-ref update-arg-names update-type)
          (let ([result (generate-update-arg-names type update-type)])
            (hash-set! update-arg-names update-type result)
            result)))
    
    (define/public (get-update-code update-type)
      (if (or (equal? update-type 'recompute)
              (not (hash-has-key? update-fns update-type)))
          (begin (when (not (equal? update-type 'recompute))
                   (printf "Warning: Using recomputation instead of update of type ~a to ~a~%" update-type id))
                 ;; TODO: Fix this, out of date
                 (hash-ref update-fns 'recompute))
          (hash-ref update-fns update-type)))

    ;; Appends the given code to the update function for the given
    ;; update type, or creates the update function if it does not
    ;; exist yet.
    (define/public (add-update-code update-type code)
      (if (hash-has-key? update-fns update-type)
          (hash-set! update-fns update-type
                     (append (hash-ref update-fns update-type) (list code)))
          (hash-set! update-fns update-type (list 'begin code))))

    ;; Wrappers around various functions on types
    
    (define/public (get-symbolic-code [varset-name #f])
      (symbolic-code type (get-id) varset-name))
    
    (define/public (get-symbolic-update-code update-type update-args)
      (symbolic-update-code type update-type (get-id) update-args))
    
    (define/public (get-old-values-code update-type . update-args)
      (apply old-values-code type update-type (get-id) update-args))
    
    (define/public (get-base-update-code update-type update-args)
      ;; TODO: Put this in the same format as get-symbolic-update-code
      (apply (update-code type update-type) update-args))))
