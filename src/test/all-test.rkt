#lang racket

(require "grammar-test.rkt" "rosette-util-test.rkt" "types-test.rkt"
         "variable-test.rkt")

(define num-errors
  (+ (run-variable-tests)
     (run-types-tests)
     (run-rosette-util-tests)
     (run-grammar-tests)))

(if (= num-errors 0)
    (displayln "Success!")
    (printf "FAILURE: ~a tests did not succeed.~%" num-errors))
