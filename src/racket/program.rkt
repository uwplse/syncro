#lang racket

(require "dependency-graph.rkt" "../rosette/variable.rkt")

(provide (struct-out program) make-program)

(struct program (constants dependency-graph algorithm) #:transparent #:mutable)

(define (make-program)
  (program '() (make-dependency-graph) '()))
