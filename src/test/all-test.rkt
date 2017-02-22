#lang racket

(require "operators-test.rkt"
         "types-test.rkt"
         "variable-test.rkt"
         "language-test.rkt"
         "grammar-test.rkt")

(define num-errors
  (+ (run-operators-tests)
     (run-types-tests)
     (run-variable-tests)
     (run-language-tests)
     (run-grammar-tests)))

(if (= num-errors 0)
    (displayln "Success!")
    (printf "FAILURE: ~a tests had problems.~%" num-errors))
