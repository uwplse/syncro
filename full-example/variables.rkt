#lang racket

(provide variable typed-variable variable-with-flags
         variable-symbol variable-definition
         (rename-out [typed-variable-type variable-type]
                     [variable-with-flags-flags variable-flags]
                     [variable-with-flags? variable-has-flags?])
         variable? typed-variable?
         with-flags)

(struct variable (symbol definition))
(struct typed-variable variable (type))
(struct variable-with-flags variable (flags))

(define (with-flags var . flags)
  (variable-with-flags (variable-symbol var)
                       (variable-definition var)
                       (typed-variable-type var)
                       (if (variable-with-flags? var)
                           (set-union (variable-with-flags-flags var)
                                      (set flags))
                           (set flags))))
