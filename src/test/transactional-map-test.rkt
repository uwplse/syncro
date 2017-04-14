#lang rosette

(require rackunit rackunit/text-ui)
(require "../rosette/transactional-map.rkt" "../rosette/types.rkt")

(provide run-transactional-map-tests)

(define tests
  (test-suite
   "Tests for transactional-map.rkt"

   (test-case "Concrete operations"
     (define test-map (make-map 5))
     (check-equal? (map-keys test-map) '())
     (check-equal? (map-values test-map) '())
     (check-false (map-has-key? test-map 'a))
     ;; Currently map-ref returns #f on failure, this may change in
     ;; the future though.
     (check-false (map-ref test-map 'foo))

     (map-set! test-map 'foo 3)
     (map-set! test-map 'bar 1)
     (check-equal? (apply set (map-keys test-map)) (set 'foo 'bar))
     (check-equal? (apply set (map-values test-map)) (set 1 3))
     (check-true (map-has-key? test-map 'foo))
     (check-true (map-has-key? test-map 'bar))
     (check-false (map-has-key? test-map 'mystery))
     (check-equal? (map-ref test-map 'foo) 3)
     (check-equal? (map-ref test-map 'bar) 1)

     (map-set! test-map 'foo 10)
     (check-equal? (apply set (map-keys test-map)) (set 'foo 'bar))
     (check-equal? (apply set (map-values test-map)) (set 1 10))
     (check-true (map-has-key? test-map 'foo))
     (check-true (map-has-key? test-map 'bar))
     (check-false (map-has-key? test-map 'mystery))
     (check-equal? (map-ref test-map 'foo) 10)
     (check-equal? (map-ref test-map 'bar) 1)

     (map-set! test-map 'a 1)
     (map-set! test-map 'c 3)
     (check-equal? (apply set (map-keys test-map))
                   (set 'foo 'bar 'a 'c))
     (check-equal? (apply set (map-values test-map))
                   (set 1 3 10))
     (check-equal? (length (map-values test-map)) 4)
     (check-exn exn:fail? (thunk (map-set! test-map 'b 2))))

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

     (define test-map (make-map 5))
     (check-equal? (map-keys test-map) '())
     (check-equal? (map-values test-map) '())
     (check-false (map-has-key? test-map 'a))

     (when (and b1 b2)
       (map-set! test-map 'a 1))
     (when (or b1 b2)
       (map-set! test-map 'b 2))

     (check-all-pcs (map-ref test-map 'a) '(1 #f #f #f))
     (check-all-pcs (map-ref test-map 'b) '(2 2 2 #f))

     (unless b1
       (map-set! test-map 'a (if (map-has-key? test-map 'b)
                                 (map-ref test-map 'b)
                                 3)))

     (check-all-pcs (map-ref test-map 'a) '(1 #f 2 3))
     (check-all-pcs (map-ref test-map 'b) '(2 2 2 #f)))

   (test-case "check-max"
     (define test-map (make-map 5))
     (define-symbolic b boolean?)
     (when b
       (map-set! test-map 'a 1)
       (map-set! test-map 'b 2)
       (map-set! test-map 'c 3)
       (map-set! test-map 'd 4)
       (map-set! test-map 'e 5))
     ;; list of keys should not be concretely empty
     (check-equal? (equal? (map-keys test-map) '()) (not b)))
   ))

(define (run-transactional-map-tests)
  (displayln "Running tests for transactional-map.rkt")
  (run-tests tests))
