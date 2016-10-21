#lang racket

(provide variable typed-variable variable-with-flags
         variable-symbol variable-definition
         (rename-out [typed-variable-type variable-type]
                     [variable-with-flags-flags variable-flags]
                     [variable-with-flags? variable-has-flags?])
         variable? typed-variable?
         with-flags)

;; symbol: A symbol representing the variable name.
;; definition: An S-expression (not syntax) that when evaluated would
;;             define the variable and give it its initial value.
;; type: The type (from types.rkt) associated with this variable
;; flags: A set of symbols. Each symbol is a flag (such as 'mutable if
;;        mutation to this variable is allowed).
(struct variable (symbol definition))
(struct typed-variable variable (type))
(struct variable-with-flags typed-variable (flags))

;; Adds flags to the given typed-variable.
(define (with-flags var . flags)
  (variable-with-flags (variable-symbol var)
                       (variable-definition var)
                       (typed-variable-type var)
                       (if (variable-with-flags? var)
                           (set-union (variable-with-flags-flags var)
                                      (apply set flags))
                           (apply set flags))))
