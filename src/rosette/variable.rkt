#lang rosette

(require "util.rkt")

(provide make-variable variable variable?
         variable-symbol variable-type variable-mutable?
         variable-expression variable-value set-variable-value!
         variable-has-type? variable-has-value? variable-has-expression?
         variable-definition variable-set!-code variable-has-definition?)


;; symbol: A symbol representing the variable name.
;; type: The type (from types.rkt) of value this variable can contain.
;; definition: An S-expression (not syntax) that when evaluated would
;;             define the variable and give it its initial value.
;; value: The value that the variable currently has.
;; mutable?: Whether or not this variable can be changed using set!
;; Note that we depend on equality doing the right thing, which only
;; happens because of #:transparent
(struct variable (symbol type [value #:mutable] mutable? expression)
  #:transparent)

;; This is copied in language.rkt for lifted variables.
(define (make-variable symbol
                       #:type [type #f]
                       #:value [value (unknown-value)]
                       #:mutable? [mutable? #f]
                       #:expression [expression #f])
  (variable symbol type value mutable? expression))

(define variable-has-type?
  (compose not false? variable-type))

(define variable-has-value?
  (compose not unknown-value? variable-value))

(define variable-has-expression?
  (compose not false? variable-expression))

(define variable-has-definition? variable-has-expression?)
(define (variable-definition var)
  (list 'define (variable-symbol var) (variable-expression var)))
(define (variable-set!-code var)
  (list 'set! (variable-symbol var) (variable-expression var)))
