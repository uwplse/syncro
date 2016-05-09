#lang racket

(require "rosette-namespace.rkt")
(require "lsl-namespace.rkt")

;; Goal is for the following program to work:
(define (assert-positive x) (assert (> x 0)))

(define-constant NUM_WORDS integer?)
(define-constant NUM_TOPICS integer?)
(define-constant NUM_DOCUMENTS integer?)
(define-constant VOCABULARY_SIZE integer?)

;; (define-base-type word (enum-range NUM_WORDS))
;; (define-base-type topic (enum-range NUM_TOPICS))
;; (define-base-type document (enum-range NUM_DOCUMENTS))

;; TODO: Need to specify types better -- specifically, how do we
;; specify the range of this relation?
(define-structure topics (Vector NUM_WORDS topic)
  (mutable))

(define-structure num1 (Vector NUM_TOPICS integer?)
  (incremental
   (lambda (t)
     (size-of-set ((word w) | (equal? (vector-ref topics w) t))))))

(define-structure num2 (Vector NUM_DOCUMENTS integer?)
  (incremental
   (lambda (d)
     (size-of-set ((topic t) | (exists (word w)
                                       (and (equal? (vector-ref topics w) t)
                                            (equal? (document w) d))))))))

(define-structure value (integer?)
  (incremental
   (lambda ()
     (sum (/ (* beta num1) (+ (* beta VOCABULARY_SIZE) (sum num2)))))))

(define (go)
  (for/sum ([i 100])
    (update-topics! (random NUM_WORDS) (random NUM_TOPICS))
    value))
