#lang racket

(provide (all-defined-out))

;; TODO: Unify constant and incremental variables, store them all in a
;; single data structure.
(define constant-terminal-list (make-parameter '()))
(define constant-terminal-types (make-hash))

;; Constant definitions contains the definitions for all the constant
;; terminals, and the untyped constants (such as type definitions)
(define constant-definitions (make-parameter '()))

(define-syntax-rule (define-constant var type val)
  (begin (when (hash-has-key? constant-terminal-types 'var)
           (error (format "Constant ~a already exists!~%" 'var)))
         (define var val)
         (constant-terminal-list (cons 'var (constant-terminal-list)))
         (hash-set! constant-terminal-types 'var type)
         (constant-definitions (cons '(define var val) (constant-definitions)))))

(define-syntax-rule (define-untyped-constant var val)
  (begin (define var val)
         (constant-definitions (cons '(define var val) (constant-definitions)))))
