#lang rosette

(require "../symhash.rkt" "../util.rkt")

(provide make-environment environment-ref environment-set)

;; We may in the future want an environment ADT that actually does
;; lexical scoping, but for now an environment just maps symbols to
;; values with no extra structure beyond that.

(define (make-environment [assocs '()])
  (rhash assocs))

(define (environment-ref env sym)
  (unless (rhash-has-key? env sym)
    (internal-error (format "Environment does not contains ~a!" sym)))
  (rhash-ref env sym))

(define (environment-set env sym val)
  (rhash-set env sym val))

