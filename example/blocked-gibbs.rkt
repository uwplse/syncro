#lang racket

;; An optimization to factored blocked Gibbs sampling -- memoizing the
;; sampling factors that would be generated to sample from each
;; possible block.

(require "../src/racket/constructs.rkt")

;;;;;;;;;;;;;;;
;; Constants ;;
;;;;;;;;;;;;;;;

(define-constant NUM_VALS (Integer-type) 4)
(define-enum-type Val NUM_VALS)

(define-constant NUM_VARS (Integer-type) 10)
(define-enum-type Var NUM_VARS)

;; For now, let's assume that factor graphs and factors are built in,
;; but blocks are not.
;; An Index is a (potentially partial) assignment of values to
;; variables Var --> Val
;; A Factor contains a list of variables and a table that maps Index
;; --> Real, where the indices are all possible assignments to the
;; given list of variables.
;; A Factor Graph is an undirected graph where each variable is
;; connected to any factors that include that variable.
(define-symbolic-constant factor-graph
  (Factor-Graph-type Var NUM_VARS Val NUM_VALS))

(define Block-type (Set-type Var))

;; Each block is a set of variables.
;; blocks contains the list of all blocks. There should be no overlap
;; in blocks.
(define-symbolic-constant blocks (List-type 10 Block-type)
  #:assume (for* ([set1 blocks]
                  [set2 blocks])
             (unless (eq? set1 set2)
               (assert (set-empty? (set-intersect set1 set2))))))

;;;;;;;;;;;;;;;;;;;;
;; Mutable values ;;
;;;;;;;;;;;;;;;;;;;;

;; Arbitrary means that we can make arbitrary changes to the state
;; (though they must still preserve the type)
(define-incremental state (Vector-type Var Val) () (arbitrary)
  (build-vector NUM_VARS (lambda (var) (random NUM_VALS))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Incremental structures ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (flatmap f lst)
  (foldl append '() (map f lst)))

;; Syntax here means sampling-factor is an incremental function that
;; takes a Block-type and produces a Factor-type. It depends on
;; the mutable data structure state.
;; The desired result is to somehow create a function sampling-factor
;; that when called will efficiently do the same thing as the code
;; given. It may assume that the value depends only on the arguments
;; to the function and the "state" data structure.
(define-incremental-fn (sampling-factor [block Block-type]) : (Factor-type Var Val) (state)
  (let ()
    (define adjacent-factors
      (flatmap (lambda (var) (get-factors factor-graph var))
               (set->list block)))

    ;; Gets a value from the factor by indexing into the factor using
    ;; the given index where possible and values from the state where
    ;; the given index does not have a value.
    (define (get-value f index)
      (factor-get f (specialize-assignment state index)))
    
    (define result (make-factor block))

    (for ([index (get-indices result)])
      (factor-set! result index
                   (product
                    (map (lambda (f) (get-value f index))
                         adjacent-factors))))))

;;;;;;;;;;;;;;;
;; Algorithm ;;
;;;;;;;;;;;;;;;

(define (main)
  (for ([i 1000])
    (for ([block blocks])
      (set-state! (specialize state
                              (sample-from-factor
                               (sampling-factor block))))))
  (displayln state))
