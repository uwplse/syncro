#lang racket

(require "rosette-util-test.rkt" "types-test.rkt" "variable-test.rkt")

(+ (run-variable-tests)
   (run-rosette-util-tests)
   (run-types-tests))
