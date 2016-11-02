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
     (test-case "Constructors, selectors and equality for types"
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

     (test-case "Predicates on types"
       (check-false (type? 12))

       (for ([t all])
         (check-true (type? t))
         (check-true (Any-type? t))
         (check-true (is-supertype? any t)))

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

     (test-case "Recursive supertyping"
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

     (test-case "Repr method"
       (for ([t all])
         ;; Special rules for Enums that are not tested here, see types.rkt
         ;; vec contains an enum, so it will also not work
         (unless (member t (list enum vec))
           (check-equal? t (eval (repr t) ns)))))

     (test-case "Unification without type variables"
       (for* ([t1 all]
              [t2 all])
         (cond [(is-supertype? t1 t2)
                (check-equal? (unify-types t1 t2) t2
                              (format "~a is a supertype of ~a but unification does not give ~a"
                                      (repr t1) (repr t2) (repr t2)))]
               [(is-supertype? t2 t1)
                (check-equal? (unify-types t1 t2) t1
                              (format "~a is a supertype of ~a but unification does not give ~a"
                                      (repr t2) (repr t1) (repr t1)))]
               [else
                (check-exn exn:fail? (lambda () (unify-types t1 t2))
                           (format "~a and ~a have no subtyping relation but unification did not fail"
                                   (repr t1) (repr t2)))])))

     (test-case "Unification with type variables"
       (match-define (list a1 a2 a3)
         (build-list 3 (lambda (i) (Type-var))))

       (check-equal? (unify-types int a1) int)
       (check-equal? (unify-types (Vector-type a1 int) (Vector-type int a2))
                     (Vector-type int int))
       (check-exn exn:fail?
                  (lambda ()
                    (unify-types (Vector-type a1 int)
                                 (Vector-type int bool))))
       (check-equal? (unify-types
                      (Procedure-type (list (Vector-type a1 a2) a1) a2)
                      (Procedure-type (list (Vector-type int a1) a3) int))
                     (Procedure-type (list (Vector-type int int) int) int))
       (check-exn exn:fail?
                  (lambda ()
                    (unify-types
                     (Procedure-type (list (Vector-type a1 a2) a1) a2)
                     (Procedure-type (list (Vector-type int a1) a3) bool)))))

     (test-case "apply-type"
       (match-define (list a1 a2 a3)
         (build-list 3 (lambda (i) (Type-var))))

       ;; Simple examples solvable with direct equality
       (check-equal? (apply-type (Procedure-type '() int) '()) int)
       (check-equal? (apply-type (Procedure-type (list int) bool)
                                 (list int))
                     bool)
       (check-exn exn:fail? (lambda ()
                              (apply-type (Procedure-type (list int) bool)
                                          (list int int))))
       (check-exn exn:fail? (lambda ()
                              (apply-type (Procedure-type (list int) bool)
                                          (list bool))))

       ;; Examples that require subtyping reasoning
       (check-equal? (apply-type (Procedure-type (list idx) bool)
                                 (list int))
                     bool)
       (check-equal? (apply-type (Procedure-type (list int) bool)
                                 (list idx))
                     bool)

       ;; Examples that require unification and subtyping
       (check-equal? (apply-type
                      (Procedure-type (list (Vector-type idx a2) a1) a2)
                      (list vec (Vector-index-type vec)))
                     (Vector-output-type vec))
       
       (check-exn exn:fail?
                  (lambda ()
                    (apply-type
                     (Procedure-type (list (Vector-type a1 a2) a1) a2)
                     (list vec bool)))))
       
     )))

;; TODO: Tests for the symbolic code generation

(define (run-types-tests)
  (displayln "Running tests for types.rkt")
  (run-tests tests))
