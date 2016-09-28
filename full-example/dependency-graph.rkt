#lang racket

(require graph)

(require "types.rkt")

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

(define (make-dependency-graph)
  (let ([g (directed-graph '())])
    (define-vertex-property g id->node)
    (dep-graph g id->node id->node-set! '())))

(define (add-node! dg id node)
  (match-let ([(dep-graph graph id->node id->node-set! id-list) dg])
    (when (has-vertex? graph id)
      (error (format "Symbol has already been used: ~a" id)))
    (add-vertex! graph id)
    (id->node-set! id node)
    (set-dep-graph-id-list! dg (append id-list (list id)))))

(define (add-dependency! dg parent child)
  (add-directed-edge! (dep-graph-graph dg) parent child))

(define (get-node dg id)
  ((dep-graph-id->node dg) id))

(define (get-ids dg)
  (dep-graph-id-list dg))

;;;;;;;;;;;
;; Nodes ;;
;;;;;;;;;;;

(define node%
  (class* object% (writable<%>)
    (super-new)

    (init-field id type update-types fn-code)
    (init-field [update-arg-names (make-hash)])

    (define update-fns (make-hash))

    (define/public (custom-write port)
      (display (format "#<structure%:~a>" id) port))

    (define/public (custom-display port)
      (display (format "#<structure%:~a>" id) port))

    (define/public (get-id) id)
    (define/public (get-type) type)
    (define/public (get-update-types) update-types)
    (define/public (get-fn-code) fn-code)

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
                 (hash-ref update-fns 'recompute))
          (hash-ref update-fns update-type)))

    (define/public (add-update-code update-type code)
      (if (hash-has-key? update-fns update-type)
          (hash-set! update-fns update-type
                     (append (hash-ref update-fns update-type) (list code)))
          (hash-set! update-fns update-type (list 'begin code))))

    (define/public (get-symbolic-code [varset-name #f])
      (symbolic-code type (get-id) varset-name))
    
    (define/public (get-symbolic-update-code update-type update-args)
      (symbolic-update-code type update-type (get-id) update-args))
    
    (define/public (get-old-values-code update-type . update-args)
      (apply old-values-code type update-type (get-id) update-args))
    
    (define/public (get-base-update-code update-type update-args)
      ;; TODO: Put this in the same format as get-symbolic-update-code
      (apply (update-code type update-type) update-args))))
