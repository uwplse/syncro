;; Provides a way to run dynamically generated Racket code.
#lang rosette

(require "namespace-requires.rkt")

(provide run-in-rosette)

(current-bitwidth 10)

(define (run-in-rosette code)
  (eval-syntax (datum->syntax #'3 code)))
