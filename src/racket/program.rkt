#lang racket

(require "dependency-graph.rkt")

(provide (struct-out program) make-program (struct-out constant))

(struct program (constants dependency-graph algorithm) #:transparent #:mutable)

(define (make-program)
  (program '() (make-dependency-graph) '()))

;; This is a layer of indirection on top of variables, that allows us
;; to easily specify what kind of constant we have, differentiating
;; between the ones created by define and define-symbolic, and also
;; differentiating between the ones that have configs and the ones
;; that don't. We could do this by testing various properties of
;; variables (eg. whether or not they have a type), but this is less
;; flexible -- if we change the properties of various constants, or
;; want to add new kinds of constants, it would be hard to do.
(struct constant (var version) #:transparent)
