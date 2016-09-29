#lang s-exp rosette

(require racket/syntax rosette/lib/angelic)

(require "types.rkt" "rosette-util.rkt")

(provide vector-sum vector-increment! vector-decrement!
         grammar Terminal-Info%
         eval-lifted lifted-code)

(define num-boolean-vars (make-parameter 0))
(define (my-choose* . args)
  (define valid-args args)
  ;; TODO: Following line sometimes speeds it up, sometimes slows it down.
  #;(define valid-args (filter (lambda (arg) (not (lifted-error? arg))) args))
  (if (null? valid-args)
      (lifted-error)
      (begin (num-boolean-vars (+ -1 (length valid-args) (num-boolean-vars)))
             (apply choose* valid-args))))

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

(match-define (list alpha-v1 alpha-v2 alpha-v3 beta-v3 alpha-v4 beta-v4 alpha-v5)
  (build-list 7 (lambda (i) (Type-var))))

(define cmp-type
  (Procedure-type (list (Integer-type) (Integer-type)) (Boolean-type)))
(define arith-type
  (Procedure-type (list (Integer-type) (Integer-type)) (Integer-type)))

(define-lifted
  [void void^ (Procedure-type '() (Void-type))]
  [vector-increment! vector-increment!^
                     (Procedure-type (list (Vector-type alpha-v1 (Integer-type))
                                           alpha-v1)
                                     (Void-type))]
  [vector-decrement! vector-decrement!^
                     (Procedure-type (list (Vector-type alpha-v2 (Integer-type))
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
                              beta-v4)]
  [equal? equal?^ (Procedure-type (list alpha-v5 alpha-v5) (Boolean-type))]
  [= =^ cmp-type] [< <^ cmp-type]
  [+ +^ arith-type] [- -^ arith-type] [* *^ arith-type] [/ /^ arith-type])

(define (grammar terminal-info num-stmts depth
                 #:num-temps [num-temps 2]
                 #:guard-depth [guard-depth #f])

  (define (stmt-grammar num-stmts depth)
    (if (= num-stmts 0)
        (void^)
        (my-choose* (void^)
                    (begin^ (base-stmt-grammar num-stmts depth)
                            (stmt-grammar (- num-stmts 1) depth)))))

  (define (base-stmt-grammar num-stmts depth)
    ;; TODO: boolean-expr-grammar should have some other depth
    (if (<= num-stmts 1)
        (vector-stmt-grammar depth)
        (my-choose* (vector-stmt-grammar depth)
                    (if^ (expr-grammar (Boolean-type) depth)
                         (stmt-grammar 1 depth)
                         (stmt-grammar 1 depth)))))

  (define (vector-stmt-grammar depth)
    (let* ([vec (expr-grammar (Vector-type (Bottom-type) (Any-type))
                              (- depth 1) #:mutable #t)]
           [vec-type (infer-type vec)])
      (if (Error-type? vec-type)
          (lifted-error)
          (let* ([index (expr-grammar (Vector-index-type vec-type) (- depth 1))]
                 [value (apply my-choose*
                               (send terminal-info get-terminals
                                     #:type (Vector-output-type vec-type)))])
            (my-choose* ((my-choose* vector-increment!^ vector-decrement!^) vec index)
                        (vector-set!^ vec index value))))))

  ;; If #:mutable is #t, then the return value must be mutable.
  ;; It is assumed that it is possible for the desired type to be mutable.
  ;; If #:mutable is #f, then the return value may or may not be mutable.
  (define (expr-grammar desired-type depth #:mutable [mutable #f])
    (let ([base (apply my-choose*
                       (send/apply terminal-info get-terminals
                                   #:type desired-type
                                   (if mutable '(mutable) '())))])
      (if (= depth 0)
          base
          (my-choose* base
                      (base-expr-grammar desired-type depth)
                      (vector-ref-expr desired-type depth #:mutable mutable)))))

  (define (vector-ref-expr desired-type depth #:mutable [mutable #f])
    ;; TODO: Force the output type to be the desired-type
    (let* ([vec (expr-grammar (Vector-type (Bottom-type) desired-type)
                              (- depth 1) #:mutable mutable)]
           [vec-type (infer-type vec)])
      (if (Error-type? vec-type)
          (lifted-error)
          (vector-ref^ vec
                       (expr-grammar (Vector-index-type vec-type)
                                     (- depth 1))))))

  (define (base-expr-grammar desired-type depth)
    (let ([options '()])
      (when (is-supertype? desired-type (Boolean-type))
        (let (#;[b1 (expr-grammar (Boolean-type) (- depth 1))]
              #;[b2 (expr-grammar (Boolean-type) (- depth 1))]
              [i1 (expr-grammar (Integer-type) (- depth 1))]
              [i2 (expr-grammar (Integer-type) (- depth 1))]
              [e1 (expr-grammar (Any-type) (- depth 1))]
              [e2 (expr-grammar (Any-type) (- depth 1))])
          (set! options
                (append options
                        (list (=^ i1 i2)
                              (<^ i1 i2)
                              (equal?^ e1 e2)
                              #;(and b1 b2)
                              #;(or b1 b2)
                              #;(not b1))))))
      (when (is-supertype? desired-type (Integer-type))
        (let ()
          (define-symbolic* hole integer?)
          (set! options (cons hole options))))

      (apply my-choose* options)))

  (num-boolean-vars 0)
  ;; Choose guard
  (define guard-expr
    (and guard-depth (expr-grammar (Boolean-type) guard-depth)))
  
  ;; Add temporary variables for common subexpression reuse.
  ;; Generate temporary variable names
  (define temps
    (build-list num-temps
                (lambda (i) (gensym 'tmp))))

  ;; Choose definitions for each variable
  (define definitions
    (for/list ([sym temps])
      (let* ([subexp (expr-grammar (Any-type) 1)]
             [result (define^ sym subexp)])
        (send terminal-info add-terminal sym (eval-lifted subexp) (infer-type subexp)
              #:mutable #f)
        result)))

  ;; Build the program
  (define result
    (apply begin^
           (append definitions (list (stmt-grammar num-stmts depth)))))

  ;; Add guards if required
  (when guard-depth
    (set! result
          (if^ guard-expr (void^) result)))
  
  (display
   (format "Used ~a boolean variables in the grammar~%" (num-boolean-vars)))
  result)

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
    (define/public (get-terminals #:type [type (Any-type)] . flags) 
      (define flags-set (list->set flags))
      (unless (subset? flags-set all-flags)
        (error (format "Unrecognized flag(s): ~a~%"
                       (set->list (set-subtract flags-set
                                                all-flags)))))
      
      (filter (lambda (terminal)
                (let ([sym-type (Terminal-type terminal)]
                      [sym-flags (Terminal-flags terminal)])
                  (and (is-supertype? type sym-type)
                       (subset? flags-set sym-flags))))
              (hash-values symbol->terminal)))))
