#lang rosette

(require rackunit rackunit/text-ui)
(require "../rosette/enum-set.rkt" "../rosette/types.rkt")

(provide run-enum-set-tests)

(define tests
  (test-suite
   "Tests for language.rkt"

   (test-case "Concrete operations"
     (define eset (enum-make-set 5))
     (check-true (enum-set-empty? eset))
     (check-equal? (enum-set-size eset) 0)
     (for ([i 5])
       (check-false (enum-set-contains? eset i)))

     (enum-set-add! eset 4)
     (enum-set-add! eset 1)
     (check-false (enum-set-empty? eset))
     (check-true (enum-set-contains? eset 4))
     (check-true (enum-set-contains? eset 1))
     (check-false (enum-set-contains? eset 2))
     (check-equal? (enum-set-size eset) 2)

     (enum-set-add! eset 4)
     (enum-set-add! eset 4)
     (check-false (enum-set-empty? eset))
     (check-true (enum-set-contains? eset 4))
     (check-true (enum-set-contains? eset 1))
     (check-false (enum-set-contains? eset 2))
     (check-equal? (enum-set-size eset) 2)

     (enum-set-remove! eset 1)
     (check-false (enum-set-empty? eset))
     (check-true (enum-set-contains? eset 4))
     (check-false (enum-set-contains? eset 1))
     (check-false (enum-set-contains? eset 2))
     (check-equal? (enum-set-size eset) 1)

     (enum-set-add! eset 2)
     (check-equal? (enum-set-size eset) 2)

     (define evens (build-enum-set 5 even?))
     (check-equal? (enum-set-size evens) 3)
     (check-true (enum-set-contains? evens 0))
     (check-true (enum-set-contains? evens 2))
     (check-true (enum-set-contains? evens 4))
     (check-false (enum-set-contains? evens 1))
     (check-false (enum-set-contains? evens 3))

     (check-equal? (enum-set-union eset evens) evens)

     (enum-set-add! eset 3)
     (define combined (enum-set-union eset evens))
     (check-equal? (enum-set-size combined) 4)
     (check-true (enum-set-contains? combined 0))
     (check-true (enum-set-contains? combined 2))
     (check-true (enum-set-contains? combined 3))
     (check-true (enum-set-contains? combined 4))
     (check-false (enum-set-contains? combined 1)))

   ;; TODO: Symbolic tests
   ))

(define (run-enum-set-tests)
  (displayln "Running tests for enum-set.rkt")
  (run-tests tests))
