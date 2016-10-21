#lang racket

(require rackunit rackunit/text-ui)
(require "../types.rkt")

(provide run-types-tests)

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))

(define tests
  (test-suite
   "Tests for types.rkt"
   (let* ([any (Any-type)]
          [bot (Bottom-type)]
          [idx (Index-type)]
          [bool (Boolean-type)]
          [int (Integer-type)]
          [enum (Enum-type 'Word 12)]
          [vec (Vector-type 10 (Boolean-type))]
          [proc (Procedure-type (list int int) (Integer-type))]
          [err (Error-type)]
          [void (Void-type)]
          [all (list any bot idx bool int enum vec proc err void)])
     (test-case
         "Constructors, selectors and equality for types"
       (check-exn exn:fail? (lambda () (Vector-type bool int)))
       (check-exn exn:fail? (lambda () (Vector-type any int)))
       
       (check-equal? bot (Bottom-type))
       (check-equal? idx (Index-type))
       (check-equal? bool (Vector-output-type vec))
       (check-equal? int (Vector-index-type vec))
       ;; Enums are only equal to themselves
       (check-not-equal? enum (Enum-type 'Word 12))
       (check-equal? enum enum)
       (check-equal? vec (Vector-type 10 bool))
       (check-equal? proc (Procedure-type (list (Integer-type) int) (Integer-type)))
       (check-equal? err (Error-type))
       (check-equal? void (Void-type))

       (check-not-equal? any bot)
       (check-not-equal? idx int)
       (check-not-equal? proc (Procedure-type (list int bool) (Integer-type)))
       (check-not-equal? vec (Vector-type 10 int)))

     (test-case
         "Predicates on types"
       (check-false (type? 12))

       (for ([t all])
         (check-true (type? t))
         (check-true (Any-type? t)))

       (define predicate->type
         (hash Bottom-type? bot
               Boolean-type? bool
               Index-type? idx
               Integer-type? int
               Enum-type? enum
               Vector-type? vec
               Procedure-type? proc
               Error-type? err
               Void-type? void))

       (define expected-true-cases
         (hash Bottom-type? (set bot)
               Boolean-type? (set bot bool)
               Index-type? (set bot idx int enum)
               Integer-type? (set bot int)
               Enum-type? (set bot enum)
               Vector-type? (set bot vec)
               Procedure-type? (set bot proc)
               Error-type? (set bot err)
               Void-type? (set bot void)))

       (for ([pred (hash-keys expected-true-cases)])
         (define expected (hash-ref expected-true-cases pred))
         (define type-for-pred (hash-ref predicate->type pred))
         (for ([t all])
           (if (set-member? expected t)
               (begin (check-true (pred t))
                      (check-true (is-supertype? type-for-pred t)))
               (begin (check-false (pred t))
                      (check-false (is-supertype? type-for-pred t))))))

       (check-true (symbolic? int))
       (check-false (mutable-structure? int))
       (check-true (symbolic? vec))
       (check-true (mutable-structure? vec))
       (check-false (symbolic? err)))

     (test-case
         "Recursive supertyping"
       (check-true (is-supertype? (Vector-type int bool)
                                  (Vector-type idx bot)))
       (check-false (is-supertype? (Vector-type idx bool)
                                   (Vector-type int bot)))
       (check-false (is-supertype? (Vector-type int bot)
                                   (Vector-type idx bool)))
       (check-false (is-supertype? (Vector-type idx bot)
                                   (Vector-type int bool)))
       
       (check-false (is-supertype? (Procedure-type (list int) int)
                                   (Procedure-type (list int int) int)))
       (check-false (is-supertype? (Procedure-type (list int int) int)
                                   (Procedure-type (list int) int)))
       
       (check-true (is-supertype? (Procedure-type (list int) bool)
                                  (Procedure-type (list any) bot)))
       (check-false (is-supertype? (Procedure-type (list any) bool)
                                   (Procedure-type (list int) bot)))
       (check-false (is-supertype? (Procedure-type (list int) bot)
                                   (Procedure-type (list any) bool)))
       (check-false (is-supertype? (Procedure-type (list any) bot)
                                   (Procedure-type (list int) bool)))

       (check-true
        (is-supertype? (Procedure-type (list (Procedure-type (list bool bool)
                                                             (Vector-type idx bool)))
                                       any)
                       (Procedure-type (list (Procedure-type (list bool bool)
                                                             (Vector-type int bool)))
                                       any)))

       (check-false
        (is-supertype? (Procedure-type (list (Procedure-type (list bool bool)
                                                             (Vector-type int bool)))
                                       any)
                       (Procedure-type (list (Procedure-type (list bool bool)
                                                             (Vector-type idx bool)))
                                       any))))

     (test-case
         "Repr method"
       (for ([t all])
         ;; Special rules for Enums that are not tested here, see types.rkt
         (unless (member t (list enum vec))
           (check-equal? t (eval (repr t) ns))))))))

(define (run-types-tests)
  (displayln "Running tests for types.rkt")
  (run-tests tests))
