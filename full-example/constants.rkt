#lang racket

(require "variables.rkt")

(provide constants define-constant define-untyped-constant)

(define constants (make-parameter '()))

(define-syntax-rule (define-constant var type val)
  (begin (define var val)
         (constants (cons (typed-variable 'var  '(define var val) type)
                          (constants)))))

(define-syntax-rule (define-untyped-constant var val)
  (begin (define var val)
         (constants (cons (variable 'var '(define var val))
                          (constants)))))
