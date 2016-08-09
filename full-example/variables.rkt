#lang racket

(provide constants define-constant define-untyped-constant
         variable typed-variable variable-with-flags
         variable-symbol variable-definition
         (rename-out [typed-variable-type variable-type]
                     [variable-with-flags-flags variable-flags]
                     [variable-with-flags? variable-has-flags?])
         typed-variable?
         with-flags)

(define constant-symbols (mutable-set))
(define constants (make-parameter '()))

(define-syntax-rule (define-constant var type val)
  (begin (when (set-member? constant-symbols 'var)
           (error (format "Constant ~a already exists!~%" 'var)))
         (define var val)
         (set-add! constant-symbols 'var)
         (constants (cons (typed-variable 'var  '(define var val) type)
                          (constants)))))

(define-syntax-rule (define-untyped-constant var val)
  (begin (define var val)
         (set-add! constant-symbols 'var)
         (constants (cons (variable 'var '(define var val))
                          (constants)))))

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
