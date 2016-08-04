#lang s-exp rosette

(require racket/syntax rosette/lib/angelic)

(require "types.rkt" "rosette-util.rkt")

(provide vector-sum vector-increment! vector-decrement!
         grammar Terminal-Info%
         eval-lifted lifted-code)

(define num-boolean-vars (make-parameter 0))
(define (my-choose* . args)
  (num-boolean-vars (+ -1 (length args) (num-boolean-vars)))
  (apply choose* args))

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

(define-lifted [void void^]
  [vector-increment! vector-increment!^] [vector-decrement! vector-decrement!^]
  [vector-set! vector-set!^] [vector-ref vector-ref^])

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

;; TODO: Gracefully handle the case where there are no arguments to my-choose*
(define (vector-stmt-grammar terminal-info depth)
  (let* ([vec (apply my-choose*
                     (send terminal-info get-terminals
                           #:type Vector%
                           'mutable))]
         [vec-type (Terminal-type vec)]
         [index (index-expr-grammar (Vectr-index-type vec-type) terminal-info (- depth 1))]
         [value (apply my-choose*
                       (send terminal-info get-terminals
                             #:type (Vectr-output-type vec-type)))])
    (my-choose* ((my-choose* vector-increment!^ vector-decrement!^) vec index)
                (vector-set!^ vec index value))))

;; TODO: Create a single expr-grammar that takes a type as input and
;; produces expressions that would create a value of that type.
(define (index-expr-grammar desired-type terminal-info depth)
  (if (= depth 0)
      (apply my-choose* (send terminal-info get-terminals
                              #:type desired-type))
      (my-choose* (apply my-choose* (send terminal-info get-terminals
                                          #:type desired-type))
                  (let* ([vec (apply my-choose*
                                     (send terminal-info get-terminals
                                           #:type Vector%))]
                         [vec-type (Terminal-type vec)])
                    (vector-ref^
                     vec
                     (index-expr-grammar (Vectr-index-type vec-type)
                                         terminal-info (- depth 1)))))))


;; Extends the lifted variable struct from rosette-util.rkt
;; This allows the Terminals to automatically be lifted so that they
;; can be used directly in the grammar.
(struct Terminal lifted-variable (type flags))
(define Terminal-symbol lifted-variable-var)
(define Terminal-value lifted-variable-val)

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
    (define/public (get-terminals #:type [type-or-pred Type%] . flags)
      (define flags-set (list->set flags))
      (unless (subset? flags-set all-flags)
        (error (format "Unrecognized flag(s): ~a~%"
                       (set->list (set-subtract flags-set
                                                all-flags)))))
      
      (filter (lambda (terminal)
                (let ([sym-type (Terminal-type terminal)]
                      [sym-flags (Terminal-flags terminal)])
                  (and (or (and (Type? type-or-pred)
                                (is-subtype? sym-type type-or-pred))
                           (and (procedure? type-or-pred)
                                (type-or-pred sym-type)))
                       (subset? flags-set sym-flags))))
              (hash-values symbol->terminal)))))
