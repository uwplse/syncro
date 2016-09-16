#lang s-exp rosette

(require racket/syntax rosette/lib/angelic)

(require "types.rkt" "rosette-util.rkt")

(provide vector-sum vector-increment! vector-decrement!
         grammar Terminal-Info%
         eval-lifted lifted-code)

(define num-boolean-vars (make-parameter 0))
(define (my-choose* . args)
  (if (null? args)
      (lifted-error)
      (begin (num-boolean-vars (+ -1 (length args) (num-boolean-vars)))
             (apply choose* args))))

(define (vector-sum vec)
  (define result 0)
  (for ([v vec])
    (set! result (+ result v)))
  result)

(define (vector-increment! vec index)
  (vector-set! vec index
               (+ (vector-ref vec index) 1)))

(define (vector-decrement! vec index)
  (vector-set! vec index
               (- (vector-ref vec index) 1)))

(define-values (alpha-v1 alpha-v2 alpha-v3 beta-v3 alpha-v4 beta-v4)
  (apply values (build-list 6 (lambda (i) (Type-var)))))

(define-lifted
  [void void^ (Procedure-type '() (Void-type))]
  [vector-increment! vector-increment!^
                     (Procedure-type (list (Vector-type alpha-v1 (Any-type))
                                           alpha-v1)
                                     (Void-type))]
  [vector-decrement! vector-decrement!^
                     (Procedure-type (list (Vector-type alpha-v2 (Any-type))
                                           alpha-v2)
                                     (Void-type))]
  [vector-set! vector-set!^
               (Procedure-type (list (Vector-type alpha-v3 beta-v3)
                                     alpha-v3
                                     beta-v3)
                               (Void-type))]
  [vector-ref vector-ref^
              (Procedure-type (list (Vector-type alpha-v4 beta-v4)
                                    alpha-v4)
                              beta-v4)])

(define (grammar terminal-info num-stmts stmt-depth)
  (num-boolean-vars 0)
  (define result (stmt-grammar terminal-info num-stmts stmt-depth))
  (display
   (format "Used ~a boolean variables in the grammar~%" (num-boolean-vars)))
  result)

(define (stmt-grammar terminal-info num-stmts stmt-depth)
  (if (= num-stmts 0)
      (void^)
      (my-choose* (void^)
                  (begin^ (base-stmt-grammar terminal-info stmt-depth)
                          (stmt-grammar terminal-info (- num-stmts 1) stmt-depth)))))

(define (base-stmt-grammar terminal-info depth)
  (vector-stmt-grammar terminal-info depth))

(define (vector-stmt-grammar terminal-info depth)
  (let* ([vec (vector-expr-grammar Vector-type? '(mutable) terminal-info (- depth 1))]
         [vec-type (infer-type vec)]
         [index (index-expr-grammar (Vector-index-type vec-type) terminal-info (- depth 1))]
         [value (apply my-choose*
                       (send terminal-info get-terminals
                             #:type (Vector-output-type vec-type)))])
    (my-choose* ((my-choose* vector-increment!^ vector-decrement!^) vec index)
                (vector-set!^ vec index value))))

(define (vector-expr-grammar desired-type desired-flags terminal-info depth)
  (let ([base (apply my-choose*
                     (send/apply terminal-info get-terminals
                                 #:type desired-type
                                 desired-flags))])
    (if (= depth 0)
        base
        (my-choose* base
                    ;; TODO: Force the output type to be a vector
                    (let* ([vec (vector-expr-grammar Vector-type? desired-flags
                                                     terminal-info (- depth 1))]
                           [vec-type (Terminal-type vec)])
                      (vector-ref^
                       vec
                       (index-expr-grammar (Vector-index-type vec-type)
                                           terminal-info (- depth 1))))))))
                    
    

;; TODO: Create a single expr-grammar that takes a type as input and
;; produces expressions that would create a value of that type.
(define (index-expr-grammar desired-type terminal-info depth)
  (let ([base (apply my-choose*
                     (send terminal-info get-terminals #:type desired-type))])
    (if (= depth 0)
        base
        (my-choose* base
                    ;; TODO: Force the chosen vector to have an output type compatible with indices
                    (let* ([vec (vector-expr-grammar Vector-type? '() terminal-info
                                                     (- depth 1))]
                           [vec-type (Terminal-type vec)])
                      (vector-ref^
                       vec
                       (index-expr-grammar (Vector-index-type vec-type)
                                           terminal-info (- depth 1))))))))


;; Extends the lifted variable struct from rosette-util.rkt
;; This allows the Terminals to automatically be lifted so that they
;; can be used directly in the grammar.
;; TODO: Unify with variables.rkt?
(struct Terminal lifted-variable (flags))
(define Terminal-symbol lifted-variable-var)
(define Terminal-value lifted-variable-val)
(define Terminal-type lifted-variable-type)

;; TODO: Switch from "list of flags" to each flag being its own separate keyword. This is because different flags have to be handled differently in the grammar. For now it works because we only care about "mutable".
(define Terminal-Info%
  (class object%
    (super-new)

    (field [symbol->terminal (make-hash)]
           [all-flags (set 'mutable 'read-only)])

    (define/public (add-terminal symbol value type #:mutable [mutable #f])
      (when (hash-has-key? symbol->terminal symbol)
        (error (format "Terminal ~a is already present!~%" symbol)))

      (hash-set! symbol->terminal symbol
                 (Terminal value symbol type 
                           (set (if mutable 'mutable 'read-only)))))

    ;; Returns the terminals which are instances of subtypes of the argument
    ;; type, and which have the associated flags.
    ;; Type is either a type, or a predicate that identifies types.
    (define/public (get-terminals #:type [type-or-pred Type?] . flags)
      (define flags-set (list->set flags))
      (unless (subset? flags-set all-flags)
        (error (format "Unrecognized flag(s): ~a~%"
                       (set->list (set-subtract flags-set
                                                all-flags)))))
      
      (filter (lambda (terminal)
                (let ([sym-type (Terminal-type terminal)]
                      [sym-flags (Terminal-flags terminal)])
                  (and (or (and (Type? type-or-pred)
                                (is-supertype? type-or-pred sym-type))
                           (and (procedure? type-or-pred)
                                (type-or-pred sym-type)))
                       (subset? flags-set sym-flags))))
              (hash-values symbol->terminal)))))
