#lang rosette

(require rackunit rackunit/text-ui)
(require "../rosette/operators.rkt")

(provide run-operators-tests)

(define tests
  (test-suite
   "Tests for grammar.rkt"

   (test-case "concrete vector operations"
     (let ([vec (vector 3 2 0 3)])
       (check-equal? (vector-sum vec) 8)
       (vector-increment! vec 0)
       (check-equal? (vector-sum vec) 9)
       (vector-decrement! vec 1)
       (check-equal? (vector-sum vec) 8)
       (check-equal? vec (vector 4 1 0 3))))

   (test-case "symbolic vector operations"
     (let ([vec (vector 3 2 0 3)])
       (define-symbolic b boolean?)
       (define true-soln (solve (assert b)))
       (define false-soln (solve (assert (not b))))
       
       (vector-increment! vec (if b 1 2))
       (vector-decrement! vec (if b 0 1))
       (check-equal? (evaluate vec true-soln) (vector 2 3 0 3))
       (check-equal? (evaluate vec false-soln) (vector 3 1 1 3))

       (vector-set! vec (if b 2 3) (if b 4 5))
       (check-equal? (evaluate (vector-sum vec) true-soln) 12)
       (check-equal? (evaluate (vector-sum vec) false-soln) 10)))))

(define (run-operators-tests)
  (displayln "Running tests for operators.rkt")
  (run-tests tests))

(module+ main
  (run-operators-tests))
