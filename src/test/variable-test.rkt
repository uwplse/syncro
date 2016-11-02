#lang racket

(require rackunit rackunit/text-ui)
(require "../variable.rkt" "../types.rkt")

(provide run-variable-tests)

(define tests
  (test-suite
   "Tests for variable.rkt"
   (let* ([var (make-variable 'x #:definition '(define x 3))]
          [int-var (make-variable 'x #:type (Integer-type))])

     (test-case "Basic variables"
       (check-false (variable? 'x))
       (check-true (variable? var))
       (check-equal? (variable-symbol var) 'x)
       (check-equal? (variable-definition var) '(define x 3)))

     (test-case "Typed variables"
       (check-false (variable-has-type? var))
       (check-true (variable-has-type? int-var))
       (check-equal? (variable-type int-var) (Integer-type)))

     ;; TODO: More tests
     )))

(define (run-variable-tests)
  (displayln "Running tests for variable.rkt")
  (run-tests tests))
