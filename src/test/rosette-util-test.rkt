#lang racket

(require rackunit rackunit/text-ui)
(require "../lifted-operators.rkt" "../rosette-util.rkt" "../types.rkt")

(provide run-rosette-util-tests)

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))

(define tests
  (test-suite
   "Tests for rosette-util.rkt"
   (let ()

     (test-case "eval-lifted and lifted-code invariant"
       (define lifted-objects
         (list (+^ 2 3)
               (if^ (=^ 3 (+^ 1 2))
                    (begin^ (void^) (*^ 9 2))
                    (void^))))
       
       (for ([obj lifted-objects])
         (check-equal? (eval-lifted obj)
                       (eval (lifted-code obj) ns))))
     )))

(define (run-rosette-util-tests)
  (displayln "Running tests for rosette-util.rkt")
  (run-tests tests))
