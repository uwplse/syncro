#lang rosette

(provide make-variable variable variable?
         variable-symbol variable-definition variable-type
         variable-value variable-flags set-variable-value!
         variable-has-type? variable-has-value? variable-has-definition?
         unknown-value unknown-value?)


;; symbol: A symbol representing the variable name.
;; type: The type (from types.rkt) of value this variable can contain.
;; definition: An S-expression (not syntax) that when evaluated would
;;             define the variable and give it its initial value.
;; type: The type (from types.rkt) associated with this variable
;; flags: A set of symbols. Each symbol is a flag (such as 'mutable if
;;        mutation to this variable is allowed).
(struct variable (symbol type [value #:mutable] definition flags) #:transparent)

(struct unknown-value () #:transparent)

;; This is copied in rosette-util.rkt for lifted variables.
(define (make-variable symbol
                       #:type [type #f]
                       #:value [value (unknown-value)]
                       #:definition [definition #f]
                       . flags)
  (variable symbol type value definition (apply set flags)))

(define variable-has-type?
  (compose not false? variable-type))

(define variable-has-value?
  (compose not unknown-value? variable-value))

(define variable-has-definition?
  (compose not false? variable-definition))
