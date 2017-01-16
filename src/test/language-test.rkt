#lang racket

(require rackunit rackunit/text-ui)
(require "../rosette/grammar/lifted-operators.rkt"
         "../rosette/grammar/language.rkt"
         "../rosette/types.rkt")

(provide run-language-tests)

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))

(define tests
  (test-suite
   "Tests for language.rkt"
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

(define (run-language-tests)
  (displayln "Running tests for language.rkt")
  (run-tests tests))
