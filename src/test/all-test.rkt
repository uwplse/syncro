#lang racket

(require "types-test.rkt" "variable-test.rkt")

(+ (run-types-tests)
   (run-variable-tests))
