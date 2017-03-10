#lang rosette

(require rackunit rackunit/text-ui)
(require "../rosette/map.rkt" "../rosette/types.rkt")

(provide run-map-tests)

(define tests
  (test-suite
   "Tests for map.rkt"

   (test-case "Symbolic operations"

     (define-symbolic b1 b2 boolean?)
     (define (check-all-pcs actual expected)
       (for ([formula (list (and      b1  b2) (and      b1  (not b2))
                            (and (not b1) b2) (and (not b1) (not b2)))]
             [result expected])
         (let* ([model (solve (assert formula))]
                [eval-actual (evaluate actual model)])
           (check-equal? eval-actual result
                         (format "Got ~a, expected ~a for condition ~a"
                                 eval-actual result formula)))))

     (define test-map (build-map 5 identity identity #f))
     (check-equal? (map-keys test-map) (range 5))
     (check-equal? (map-values test-map) (range 5))
     (check-false (map-has-key? test-map 'a))
     (check-true (map-has-key? test-map 2))
     (for ([i 5])
       (check-equal? (map-ref test-map i) i))

     (when (and b1 b2)
       (map-set! test-map 1 'a))
     (when (or b1 b2)
       (map-set! test-map 2 'b))

     (check-all-pcs (map-ref test-map 1) '(a 1 1 1))
     (check-all-pcs (map-ref test-map 2) '(b b b 2))

     (unless b1
       (map-set! test-map 1 (if (equal? (map-ref test-map 2) 'b)
                                'd
                                'c)))

     (check-all-pcs (map-ref test-map 1) '(a 1 d c))
     (check-all-pcs (map-ref test-map 2) '(b b b 2)))))

(define (run-map-tests)
  (displayln "Running tests for map.rkt")
  (run-tests tests))
