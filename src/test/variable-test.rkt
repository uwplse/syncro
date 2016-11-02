#lang racket

(require rackunit rackunit/text-ui)
(require "../variable.rkt" "../types.rkt")

(provide run-variable-tests)

(define tests
  (test-suite
   "Tests for variable.rkt"
   (let* ([var (variable 'x '(define x 3))]
          [int-var (typed-variable (variable-symbol var) (variable-definition var)
                                   (Integer-type))]
          [mutable-var (with-flags int-var 'mutable)]
          [many-flags-var (with-flags mutable-var 'flag1 'flag2 'flag3)])

     (test-case "Basic variables"
       (check-false (variable? 'x))
       (check-true (variable? var))
       (check-equal? (variable-symbol var) 'x)
       (check-equal? (variable-symbol many-flags-var) 'x)
       (check-equal? (variable-definition var) '(define x 3)))

     (test-case "Typed variables"
       (check-false (typed-variable? var))
       (check-true (typed-variable? mutable-var))
       (check-exn exn:fail? (lambda () (variable-type var)))
       (check-equal? (variable-type int-var) (Integer-type))
       (check-equal? (variable-type many-flags-var) (Integer-type)))

     (test-case "with-flags"
       (check-false (variable-has-flags? int-var))
       (check-true (variable-has-flags? mutable-var))
       (check-exn exn:fail? (lambda () (variable-flags int-var)))
       (check-equal? (variable-flags mutable-var) (set 'mutable))
       (check-equal? (variable-flags many-flags-var)
                     (set 'mutable 'flag1 'flag2 'flag3))))))

(define (run-variable-tests)
  (displayln "Running tests for variable.rkt")
  (run-tests tests))
