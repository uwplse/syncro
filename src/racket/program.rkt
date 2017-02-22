#lang racket

(provide (struct-out program))

(struct program (initialization constants dependency-graph algorithm) #:transparent #:mutable)
