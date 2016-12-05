#lang racket

(require "enum-set.rkt" "types.rkt")

(require (for-syntax "dependency-graph.rkt" "grammar.rkt" "rosette-namespace.rkt"
                     "synthesis.rkt" "types.rkt" "variable.rkt"
                     racket/match
                     (only-in racket pretty-print send send/apply new)))

(provide (except-out (all-from-out "types.rkt") Enum-type)
         ;; TODO: Remove the "enum-" prefix
         (all-from-out "enum-set.rkt")
         define-enum-type
         define-constant define-incremental finalize
         my-for/sum my-for/or)

;; TODO: Refactor so that variables (symbolic or not) are attached to their types

(begin-for-syntax
  ;; Wrappers around dependency graphs to handle syntax inputs
  
  (define dependency-graph (make-dependency-graph))

  (define (add-node-to-graph! name-stx type-stx updates-stx expr-stx)
    (match-define (list name type-expr updates expr)
      (map syntax->datum (list name-stx type-stx updates-stx expr-stx)))
    
    (add-node! dependency-graph name
               (new node%
                    [id name]
                    [type (run-in-rosette type-expr)]
                    [update-types updates]
                    [fn-code expr])))

  (define (add-dependencies! parents-stx name-stx)
    (let ([parents (syntax->datum parents-stx)]
          [name (syntax->datum name-stx)])
      (for-each (lambda (parent) (add-dependency! dependency-graph parent name))
                parents)))

  ;; Collects the list of variables and typed-variables created by
  ;; define-constant and its variants
  (define constants (make-parameter '())))

;; Defines a typed constant in both Racket and Rosette, and stores
;; metadata in constants.
(define-syntax (define-constant stx)
  (syntax-case stx ()
    [(_ var type-exp val)
     (begin
       (define type (run-in-rosette (syntax->datum #'type-exp)))
       (constants
        (cons (make-variable (syntax->datum #'var)
                             #:type type
                             #:definition (syntax->datum #'(define var val)))
                        (constants)))
       (run-in-rosette (syntax->datum #'(define var val)))
       (syntax/loc stx
         (define var val)))]))

;; Defines an untyped constant in both Racket and Rosette, and stores
;; metadata in constants.
(define-syntax (define-untyped-constant stx)
  (syntax-case stx ()
    [(_ var val)
     (begin
       (constants
        (cons (make-variable (syntax->datum #'var)
                             #:definition (syntax->datum #'(define var val)))
              (constants)))
       (run-in-rosette (syntax->datum #'(define var val)))
       (syntax/loc stx
         (define var val)))]))

;; Different Enums should not be able to replace each other. So, in
;; the subtype method we compare Enums by identity. However if the
;; programmer keeps creating new types by saying (Enum-type 10) over
;; and over instead of creating a single type and using it
;; consistently, then we would get weird behavior.
;; To prevent this, Enum-type expands to a define, so as to force the
;; programmer to give the type a name. This makes it obvious that they
;; are supposed to reuse that name in the program.
(define-syntax-rule (define-enum-type name items)
  (define-untyped-constant name (Enum-type 'name items)))

;; Adds metadata to the dependency graph, and defines the data
;; structure in Racket.
(define-syntax (define-incremental stx)
  (syntax-case stx ()
    [(_ name type-exp (parent ...) (update-type ...) expr ...)
     (begin
       (add-node-to-graph! #'name #'type-exp #'(update-type ...) #'(begin expr ...))
       (add-dependencies! #'(parent ...) #'name)
       (syntax/loc stx
         (begin
           (define name (begin expr ...))
           (void))))]))

;; Performs synthesis and defines the relevant update functions.
(define-syntax (finalize stx)
  (syntax-case stx ()
    [(_)
     (begin
       (with-syntax ([(define-update-fn ...)
                      (map (lambda (x) (datum->syntax stx x))
                           (perform-synthesis (reverse (constants))
                                              dependency-graph))])
         (syntax/loc stx
           (void) #;(begin define-update-fn ...))))]))

;; NOTE: The reimplementations of for can also be found in
;; rosette-namespace.rkt
(define-syntax (my-for/sum stx)
  (syntax-case stx ()
    [(_ ([i itr] ...) expr ...)
     (syntax/loc stx
       (let ([sum 0])
         (for ([i itr] ...)
           (set! sum (+ sum (begin expr ...))))
         sum))]))

;; NOTE: See caveat in rosette-namespace.rkt
(define-syntax (my-for/or stx)
  (syntax-case stx ()
    [(_ ([i itr] ...) expr ...)
     (syntax/loc stx
       (let ([val #f])
         (for ([i itr] ...)
           (set! val (or val (begin expr ...))))
         val))]))
