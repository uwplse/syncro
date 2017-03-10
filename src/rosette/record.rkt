#lang rosette

(require "map.rkt")

(provide make-record get-field set-field!)

;; Records
;; A record is represented as a hash mapping field names to values.
;; Field names are symbols and must be known at compile time, as in
;; most imperative languages.
;; TODO: Reimplement by using for/all on keys, and using hash-set! to
;; add a guarded version of the value to the hash map. Alternatively
;; use Racket's built-in struct somehow.
(define (make-record fields values)
  (build-map (length fields) (curry list-ref fields) (curry list-ref values) #f))

(define (get-field record field-name)
  (map-ref record field-name))

(define (set-field! record field-name value)
  (map-set! record field-name value))
