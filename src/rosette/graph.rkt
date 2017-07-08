#lang rosette

(require "enum-set.rkt" "util.rkt")

;; Directed graphs

(provide make-graph make-symbolic-graph
         has-edge? add-edge! remove-edge!
         vertex-parents vertex-children)

;; A directed graph is represented as a vector of enum-sets.
;; graph[i] is the set of children of node i.
;; We break the abstraction barrier and assume the internal
;; representation of an enum set is a vector of booleans in various
;; places in this file.
(define (make-graph num-vars)
  (for/vector #:length num-vars ([i num-vars])
    (enum-make-set num-vars)))

(define (make-symbolic-graph num-vars [varset #f]
                             #:no-self-edges? [no-self-edges? #t]
                             #:acyclic? [acyclic? #f])
  (when (symbolic? num-vars)
    (internal-error
     (format "make-symbolic-graph: num-vars is not concrete: ~a" num-vars)))

  ;; Note: We break the abstraction of enum-sets here.
  (for/vector #:length num-vars ([parent num-vars])
    (for/vector #:length num-vars ([child num-vars])
      (if (or (and no-self-edges? (= parent child))
              (and acyclic? (>= parent child)))
          #f
          (begin
            (define-symbolic* edge boolean?)
            (when varset (set-add! varset (make-input edge '())))
            edge)))))

(define (has-edge? graph parent child)
  (enum-set-contains? (vector-ref graph parent) child))

(define (add-edge! graph parent child)
  (enum-set-add! (vector-ref graph parent) child))

(define (remove-edge! graph parent child)
  (enum-set-remove! (vector-ref graph parent) child))

(define (vertex-parents graph v)
  ;; Need to use for/all here so that (vector-length graph) will be
  ;; concrete, which allows us to use build-enum-set.
  (for/all ([graph graph])
    (build-enum-set (vector-length graph)
                    (lambda (parent) (has-edge? graph parent v)))))
  ;; (filter (lambda (parent) (has-edge? graph parent v))
  ;;         (build-list (vector-length graph) identity)))

(define (vertex-children graph v)
  (vector-ref graph v))
  ;; (filter (lambda (child) (has-edge? graph v child))
  ;;         (build-list (vector-length graph) identity)))
