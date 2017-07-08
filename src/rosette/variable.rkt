#lang rosette

(require "util.rkt")

(provide (struct-out variable) make-variable
         variable-has-type? variable-has-expression?
         variable-has-definition? variable-definition variable-set!-code

         (struct-out constant) make-constant config-constant?)

;; symbol: A symbol representing the variable name.
;; type: The type (from types.rkt) of value this variable can contain.
;; definition: An S-expression (not syntax) that when evaluated would
;;             define the variable and give it its initial value.
;; value: The value that the variable currently has.
;; mutable?: Whether or not this variable can be changed using set!
;; Note that we depend on equality doing the right thing, which only
;; happens because of #:transparent
(struct variable (symbol type mutable? expression)
  #:transparent)

;; This is copied in language.rkt for lifted variables.
(define (make-variable symbol
                       #:type [type #f]
                       #:mutable? [mutable? #f]
                       #:expression [expression #f])
  (variable symbol type mutable? expression))

(define variable-has-type?
  (compose not false? variable-type))

(define variable-has-expression?
  (compose not false? variable-expression))

(define variable-has-definition? variable-has-expression?)
(define (variable-definition var)
  (list 'define (variable-symbol var) (variable-expression var)))
(define (variable-set!-code var)
  (list 'set! (variable-symbol var) (variable-expression var)))


;; This is used by the Racket parsing code to support configurations,
;; which can have multiple concrete values. It's added here because I
;; don't know whether it's okay to have a Racket struct that has a
;; Rosette struct at its parent.
(struct constant variable (configs for-types?) #:transparent)
(define (make-constant symbol
                       #:type [type #f]
                       #:mutable? [mutable? #f]
                       #:expression [expression #f]
                       #:configs [configs #f]
                       #:for-types? [for-types? #f])
  (when (and expression configs)
    (internal-error "Cannot have a constant with both #:expression and #:configs!"))
  (constant symbol type mutable? expression configs for-types?))

(define (config-constant? c)
  (not (false? (constant-configs c))))
