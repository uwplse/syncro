#lang rosette

(provide make-variable variable variable?
         variable-symbol variable-type variable-mutable?
         variable-definition variable-value set-variable-value!
         variable-has-type? variable-has-value? variable-has-definition?
         unknown-value unknown-value?)


;; symbol: A symbol representing the variable name.
;; type: The type (from types.rkt) of value this variable can contain.
;; definition: An S-expression (not syntax) that when evaluated would
;;             define the variable and give it its initial value.
;; value: The value that the variable currently has.
;; mutable?: Whether or not this variable can be changed using set!
(struct variable (symbol type [value #:mutable] mutable? definition)
  #:transparent)

(struct unknown-value () #:transparent)

;; This is copied in rosette-util.rkt for lifted variables.
(define (make-variable symbol
                       #:type [type #f]
                       #:value [value (unknown-value)]
                       #:mutable? [mutable? #f]
                       #:definition [definition #f])
  (variable symbol type value mutable? definition))

(define variable-has-type?
  (compose not false? variable-type))

(define variable-has-value?
  (compose not unknown-value? variable-value))

(define variable-has-definition?
  (compose not false? variable-definition))
