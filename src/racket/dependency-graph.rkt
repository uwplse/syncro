#lang racket

(require graph)

(require "../rosette/types.rkt" "../rosette/variable.rkt")

(provide make-dependency-graph node%
         add-node! add-dependency!
         get-node get-ids get-node-for-update
         make-update-info)

;;;;;;;;;;;;;;;;;;;;;;
;; Dependency graph ;;
;;;;;;;;;;;;;;;;;;;;;;

;; A dependency graph consists of a graph, a mapping from vertices of
;; the graph to nodes containing more information, and a list of
;; variables in the order they were added to the graph.
;; The graph can give us a list of variables but does not guarantee order.
;; The underlying graph implementation stores vertices by ids.
;; We add a "node property" called id->node to the graph. This lets us
;; tell the graph to associate each vertex with a node. Whenever we
;; make a new node, we must call the generated setter id->node-set!.
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

(define update-name->node (make-hash))
(define get-node-for-update (curry hash-ref update-name->node))

(define node%
  (class* object% (writable<%>)
    (super-new)

    ;; id:                Name of the variable
    ;; type:              Type of the variable
    ;; invariants:        Expression to get invariants about the node
    ;; update-names:      The names of allowed updates
    ;; update-name->info: Required information for each update
    ;; init-code:         Code to initialize the value of the data structure
    ;; fn-code:           Expression to recompute the value of the node
    (init-field id type invariants update-name->info init-code fn-code
                [sketches (make-hash)])

    (for ([update-name (hash-keys update-name->info)])
      ;; This only checks that the update has not been used by some
      ;; other node. Since this class only takes a hash from
      ;; update-name to info, it is guaranteed that the update names
      ;; are unique (since a hash can't have two of the same key).
      (when (hash-has-key? update-name->node update-name)
        (error (format "Duplicate update: ~a" update-name)))
      (hash-set! update-name->node update-name this))

    ;; Maps each update name to its incremental update function
    ;; (represented as an S-expression as a nested list).
    (define update-fns (make-hash))

    (define/public (custom-write port)
      (display (format "#<structure%:~a>" id) port))

    (define/public (custom-display port)
      (display (format "#<structure%:~a>" id) port))

    (define/public (get-id) id)
    (define/public (get-type) type)
    (define/public (get-invariants-code) invariants)
    (define/public (get-fn-code) fn-code)

    (define/public (has-sketch? update-name)
      (hash-has-key? sketches update-name))
    (define/public (get-sketch update-name)
      (hash-ref sketches update-name))
    (define/public (set-sketch! update-name sketch)
      (hash-set! sketches update-name sketch))

    (define/public (get-update-names)
      (hash-keys update-name->info))

    (define/public (get-update-args update-name)
      (update-info-args (hash-ref update-name->info update-name)))
    (define/public (get-update-arg-names update-name)
      (map variable-symbol (get-update-args update-name)))

    (define/public (get-update-body update-name)
      (update-info-body (hash-ref update-name->info update-name)))

    ;; Checks that the given names are consistent with the existing ones.
    ;; If no names exist, the given names are set as the update arg names.
    (define/public (assert-update-arg-names! update-name names)
      (let ([old-names (get-update-arg-names update-name)])
        (unless (equal? old-names names)
          (error (format "For the ~a update to ~a, there are two different sets of names: ~a and ~a"
                         update-name id old-names names)))))
    
    (define/public (get-update-code update-name)
      (if (or (equal? update-name 'recompute)
              (not (hash-has-key? update-fns update-name)))
          (begin (when (not (equal? update-name 'recompute))
                   (printf "Warning: Using recomputation instead of update ~a to ~a~%" update-name id))
                 `(set! ,id ,(get-fn-code)))
          (hash-ref update-fns update-name)))

    ;; Appends the given code to the update function for the given
    ;; update name, or creates the update function if it does not
    ;; exist yet.
    (define/public (add-update-code update-name code)
      (if (hash-has-key? update-fns update-name)
          (hash-set! update-fns update-name
                     (append (hash-ref update-fns update-name) (list code)))
          (hash-set! update-fns update-name (list 'begin code))))))

;;;;;;;;;;;;;;;;;;;;;;
;; Update functions ;;
;;;;;;;;;;;;;;;;;;;;;;

;; args: List of variables (from variable.rkt)
;; body: List of S-expressions (that is, nested list)
(struct update-info (args body) #:transparent)

(define (make-update-info arg-names arg-types body)
  (update-info (map (lambda (name type) (make-variable name #:type type))
                    arg-names arg-types)
               body))
