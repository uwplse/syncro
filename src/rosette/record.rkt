#lang rosette

(require "symhash.rkt")

(provide make-record get-field set-field!)

;; Records
;; A record is represented as a hash mapping field names to values.
;; Field names are symbols and must be known at compile time, as in
;; most imperative languages.
;; TODO: Reimplement by using for/all on keys, and using hash-set! to
;; add a guarded version of the value to the hash map. Alternatively
;; use Racket's built-in struct somehow.
(define (make-record assocs)
  (box (rhash assocs)))

(define (get-field record field-name)
  (rhash-ref (unbox record) field-name))

(define (set-field! record field-name value)
  (set-box! record (rhash-set (unbox record) field-name value)))
