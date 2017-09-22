#lang racket

(require graph)

(require "../rosette/types.rkt" "../rosette/variable.rkt")

(provide make-dependency-graph node%
         add-node! add-dependency! check-path?
         get-node get-ids get-node-for-delta
         make-delta-info)

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

;; dg: dep-graph
;; from-id: start the bfs in this node
;; Returns a lambda that takes an argument and check if it belongs in the path
(define (check-path? dg from-id)
  (define-values (dist _) (bfs (dep-graph-graph dg) from-id))
  (lambda (to-id)
    (and (not (eq? to-id from-id))
         (and (hash-has-key? dist to-id)
              (not (= (hash-ref dist to-id) +inf.0))))))

;;;;;;;;;;;
;; Nodes ;;
;;;;;;;;;;;

(define delta-name->node (make-hash))
(define get-node-for-delta (curry hash-ref delta-name->node))

(define node%
  (class* object% (writable<%>)
    (super-new)

    ;; id:                Name of the variable
    ;; type:              Type of the variable
    ;; invariant:         Expression to get invariant about the node
    ;; delta-names:       The names of allowed deltas
    ;; delta-name->info:  Required information for each delta
    ;; init-code:         Code to initialize the value of the data structure
    ;; fn-code:           Expression to recompute the value of the node
    (init-field id type invariant delta-name->info init-code fn-code
                [sketches (make-hash)])

    (for ([delta-name (hash-keys delta-name->info)])
      ;; This only checks that the delta has not been used by some
      ;; other node. Since this class only takes a hash from
      ;; delta-name to info, it is guaranteed that the delta names
      ;; are unique (since a hash can't have two of the same key).
      (when (hash-has-key? delta-name->node delta-name)
        (error (format "Duplicate delta: ~a" delta-name)))
      (hash-set! delta-name->node delta-name this))

    ;; Maps each delta name to its incremental delta function
    ;; (represented as an S-expression as a nested list).
    (define delta-fns (make-hash))

    (define/public (custom-write port)
      (display (format "#<structure%:~a>" id) port))

    (define/public (custom-display port)
      (display (format "#<structure%:~a>" id) port))

    (define/public (get-id) id)
    (define/public (get-type) type)
    (define/public (get-invariant-code) invariant)
    (define/public (get-fn-code) fn-code)

    (define/public (has-sketch? delta-name)
      (hash-has-key? sketches delta-name))
    (define/public (get-sketch delta-name)
      (hash-ref sketches delta-name))
    (define/public (set-sketch! delta-name sketch)
      (hash-set! sketches delta-name sketch))

    (define/public (get-delta-names)
      (hash-keys delta-name->info))

    (define/public (get-delta-args delta-name)
      (delta-info-args (hash-ref delta-name->info delta-name)))
    (define/public (get-delta-arg-names delta-name)
      (map variable-symbol (get-delta-args delta-name)))

    (define/public (get-delta-body delta-name)
      (delta-info-body (hash-ref delta-name->info delta-name)))

    ;; Checks that the given names are consistent with the existing ones.
    ;; If no names exist, the given names are set as the delta arg names.
    (define/public (assert-delta-arg-names! delta-name names)
      (let ([old-names (get-delta-arg-names delta-name)])
        (unless (equal? old-names names)
          (error (format "For the ~a delta to ~a, there are two different sets of names: ~a and ~a"
                         delta-name id old-names names)))))
    
    (define/public (get-delta-code delta-name)
      (if (or (equal? delta-name 'recompute)
              (not (hash-has-key? delta-fns delta-name)))
          (begin (when (not (equal? delta-name 'recompute))
                   (printf "Warning: Using recomputation instead of delta ~a to ~a~%" delta-name id))
                 (if (eq? (get-fn-code) #f) `()
                 `(set! ,id ,(get-fn-code))))
          (hash-ref delta-fns delta-name)))

    ;; Appends the given code to the delta function for the given
    ;; delta name, or creates the delta function if it does not
    ;; exist yet.
    (define/public (add-delta-code delta-name code)
      (if (hash-has-key? delta-fns delta-name)
          (hash-set! delta-fns delta-name
                     (append (hash-ref delta-fns delta-name) (list code)))
          (hash-set! delta-fns delta-name (list 'begin code))))))

;;;;;;;;;;;;;;;;;;;;;
;; Delta functions ;;
;;;;;;;;;;;;;;;;;;;;;

;; args: List of variables (from variable.rkt)
;; body: List of S-expressions (that is, nested list)
(struct delta-info (args body) #:transparent)

(define (make-delta-info arg-names arg-types body)
  (delta-info (map (lambda (name type) (make-variable name #:type type))
                   arg-names arg-types)
              body))
