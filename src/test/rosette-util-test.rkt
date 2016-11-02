#lang racket

(require rackunit rackunit/text-ui)
(require "../rosette-util.rkt" "../types.rkt")

(provide run-rosette-util-tests)

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))

(define tests
  (test-suite
   "Tests for rosette-util.rkt"
   (let ()

     (match-define (list alpha-v1 beta-v1 alpha-v2 beta-v2)
       (build-list 4 (lambda (i) (Type-var))))
     
     (define cmp-type
       (Procedure-type (list (Integer-type) (Integer-type)) (Boolean-type)))
     (define arith-type
       (Procedure-type (list (Integer-type) (Integer-type)) (Integer-type)))

     (define-lifted
       [void void^ (Procedure-type '() (Void-type))]
       [vector-set! vector-set!^
                    (Procedure-type (list (Vector-type alpha-v1 beta-v1)
                                          alpha-v1
                                          beta-v1)
                                    (Void-type))]
       [vector-ref vector-ref^
                   (Procedure-type (list (Vector-type alpha-v2 beta-v2)
                                         alpha-v2)
                                   beta-v2)]
       [= =^ cmp-type] [< <^ cmp-type]
       [+ +^ arith-type] [- -^ arith-type] [* *^ arith-type])

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
