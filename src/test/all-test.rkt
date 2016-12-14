#lang racket

(require "operators-test.rkt"
         "types-test.rkt"
         "variable-test.rkt"
         "rosette-util-test.rkt"
         "grammar-test.rkt")

(define num-errors
  (+ (run-operators-tests)
     (run-types-tests)
     (run-variable-tests)
     (run-rosette-util-tests)
     (run-grammar-tests)))

(if (= num-errors 0)
    (displayln "Success!")
    (printf "FAILURE: ~a tests did not succeed.~%" num-errors))
