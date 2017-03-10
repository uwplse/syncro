#lang rosette

(require "language.rkt" "lifted-operators.rkt"
         "../types.rkt" "../variable.rkt")

(provide
 ;; Generic method information
 gen:grammar-operator grammar-operator?
 ;; The generic functions
 operator-id can-use-operator?
 operator-domain-with-mutability operator-make-lifted
 ;; Creating new operators
 make-operator
 ;; Lists of operators that can be used
 default-operators extra-operators all-operators

 ;; The actual operators
 grm-if^ grm-set!^ grm-get-field^ grm-set-field!^)

;;;;;;;;;;;;;;;;;;;;;;;
;; Grammar Operators ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; The general grammar can handle anything that can:
;; 1. Tell it which subexpressions it needs to generate (by providing
;;    pairs of types and a mutability flag)
;; 2. Generate a lifted AST node given lifted AST nodes for inputs.
(define-generics grammar-operator
  ;; Identifies what kind of operator this is. May not be unique.
  (operator-id grammar-operator)
  ;; Given various information about the grammar, decide whether you
  ;; are allowed to use this operator as the next AST node.
  ;; TODO(correctness): Heuristics can interact badly with caching.
  (can-use-operator? grammar-operator orig-params curr-depth curr-type)
  ;; Given types for the inputs, produces the type of the output, or
  ;; #f if the operator cannot be applied to these types.
  (operator-domain-with-mutability grammar-operator range-type mutable?)
  ;; Given lifted programs for the inputs, produces a lifted type of the output.
  (operator-make-lifted grammar-operator inputs)

  #:defaults
  ([(lambda (x)
      (and (lifted-variable? x) (Procedure-type? (variable-type x))))

    (define (operator-id self) (variable-symbol self))

    (define (can-use-operator? self . args) #t)

    (define (operator-domain-with-mutability var range-type mutable?)
      (get-domain-given-range-with-mutability (variable-type var)
                                              range-type mutable?))

    (define (operator-make-lifted var inputs)
      (apply var inputs))]))

;; A basic operator supplies a constructor to make a lifted AST node
;; and a Procedure type with which to determine domains and
;; mutability.
;; They can provide a set of flags to control grammar generation.
;; NOTE: Flags must be carefully vetted to ensure they work okay with
;; caching, as the caching does not know about flags at all.
;; Currently supported flags: toplevel
(struct basic-operator (id type constructor flags) #:transparent
  #:methods gen:grammar-operator
  [(define (operator-id self)
     (basic-operator-id self))

   (define (can-use-operator? self orig-params curr-depth curr-type)
     (or (not (set-member? (basic-operator-flags self) 'toplevel))
         (= (hash-ref orig-params 'expr-depth) curr-depth)))

   (define (operator-domain-with-mutability self range-type mutable?)
     (get-domain-given-range-with-mutability
      (basic-operator-type self) range-type mutable?))

   (define (operator-make-lifted self inputs)
     (apply (basic-operator-constructor self) inputs))])

(define (make-operator id type constructor . flags)
  (basic-operator id type constructor (apply set flags)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific Operators ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define grm-if^
  ;; We only want if to be used for statements, not expressions
  (let ([if-type (Procedure-type (list (Boolean-type) (Void-type) (Void-type))
                                 (Void-type))])
    (make-operator 'if if-type if^ 'toplevel)))

;; Some ASTs need to be handled specially by the grammar. These must
;; be represented as symbols.
(define grm-set!^ 'set!)
(define grm-get-field^ 'get-field)
(define grm-set-field!^ 'set-field!)

(define default-operators
  (list grm-if^ grm-set!^ grm-get-field^ grm-set-field!^
        void^ vector-increment!^ vector-decrement!^ vector-set!^ vector-ref^
        enum-set-add!^ enum-set-remove!^ enum-set-contains?^
        add-edge!^ remove-edge!^ has-edge?^
        equal?^ =^ <^ +^ -^ *^ #;/^))

;; Non constant time operators that we normally don't use in grammars
;; but could in principle be used. These operators can also be used in
;; sketches.
(define extra-operators
  (list vertex-parents^ vertex-children^))

(define all-operators (append default-operators extra-operators))
