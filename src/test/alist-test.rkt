#lang rosette

(require rackunit rackunit/text-ui)
(require "../alist.rkt")

(provide run-alist-tests)

(define tests
  (test-suite
   "Tests for alist.rkt"
   (test-case "Concrete alist tests"
     (define sym->num (make-alist))

     (define (check-get key value)
       (check-true (alist-has-key? sym->num key))
       (if (list? value)
           (check-not-false (member (alist-get sym->num key) value))
           (check-equal? (alist-get sym->num key) value)))

     (define (check-get-and-remove key value)
       (check-true (alist-has-key? sym->num key))
       (match-let ([(list res new-sym->num)
                    (alist-get-and-remove sym->num key)])
         (if (list? value)
             (check-not-false (member res value))
             (check-equal? res value))
         new-sym->num))

     (define (check-not-present key)
       (check-false (alist-has-key? sym->num key))
       (check-exn exn:fail?
                  (lambda ()
                    (alist-get sym->num key)))
       (check-exn exn:fail?
                  (lambda ()
                    (alist-get-and-remove sym->num key))))
     
     (set! sym->num (alist-insert sym->num 'x 3))
     (check-get 'x 3)
     (check-exn exn:fail?
                (lambda () (alist-get sym->num 'foo)))
     (check-exn exn:fail?
                (lambda () (alist-get-and-remove sym->num 'foo)))
     
     (set! sym->num (alist-insert sym->num 'x 6))
     (set! sym->num (alist-insert sym->num 'foo 5))
     (set! sym->num (alist-insert sym->num 'x 9))
     (set! sym->num (alist-insert sym->num 'y 3))
     (check-get 'foo 5)
     ;; Get with x can return any of 3, 6, and 9
     (check-get 'x '(3 6 9))
     (check-get 'y 3)
     
     (check-get-and-remove 'y 3)
     ;; alist-get-and-remove is non-mutating
     (check-get 'y 3)
     (set! sym->num (check-get-and-remove 'foo 5))
     (check-not-present 'foo)
     ;; Remove all copies of x
     (for ([i 3])
       (set! sym->num (check-get-and-remove 'x '(3 6 9))))
     (check-not-present 'x)
     (set! sym->num (alist-insert sym->num 'x 10))
     (check-get 'x 10)

     ;; Remove everything
     (set! sym->num (check-get-and-remove 'y 3))
     (check-not-present 'y)
     (set! sym->num (check-get-and-remove 'x 10))
     (check-not-present 'x)
     (check-not-present 'foo))
   
   (test-case "Symbolic alist tests"
     (define sym->num (make-alist))
     (define-symbolic b1 b2 boolean?)

     (define (check-equal-with-assertion assertion actual expected)
       (define synth (solve (assert assertion)))
       (check-equal?
        (evaluate actual synth) expected
        (format "Assertion: ~a Actual: ~a Expected: ~a"
                assertion (evaluate actual synth) expected)))

     (define (check-all-results r1 r2 r3 r4)
       (define result
         (map (lambda (sym)
                (and (alist-has-key? sym->num sym)
                     (alist-get sym->num sym)))
              '(x y)))
       
       (check-equal-with-assertion (and     b1        b2   )  result r1)
       (check-equal-with-assertion (and     b1     (not b2))  result r2)
       (check-equal-with-assertion (and  (not b1)     b2   )  result r3)
       (check-equal-with-assertion (and  (not b1)  (not b2))  result r4))

     (define (get-and-remove key)
       (if (alist-has-key? sym->num key)
           (match-let ([(list res new-sym->num)
                        (alist-get-and-remove sym->num key)])
             new-sym->num)
           sym->num))
     
     (set! sym->num
           (if b1
               (alist-insert sym->num 'x 3)
               (alist-insert sym->num (if b2 'x 'y) 5)))
     (check-all-results '(3 #f) '(3 #f) '(5 #f) '(#f 5))
     
     (when b1
       (set! sym->num (get-and-remove 'x)))
     (check-all-results '(#f #f) '(#f #f) '(5 #f) '(#f 5))

     ;; Here we assume that when inserting multiple values with the
     ;; same key into an alist, it behaves like a stack. This is not
     ;; actually required by the interface but matches the current
     ;; semantics.
     (set! sym->num (if (and b1 b2)
                        (alist-insert sym->num 'x 6)
                        (alist-insert sym->num 'x 7)))
     (check-all-results '(6 #f) '(7 #f) '(7 #f) '(7 5))

     (when (or b1 b2)
       (set! sym->num (alist-insert sym->num 'y 8)))
     (check-all-results '(6 8) '(7 8) '(7 8) '(7 5))

     (unless b1
       (set! sym->num (get-and-remove 'x)))
     (check-all-results '(6 8) '(7 8) '(5 8) '(#f 5))
     
     (set! sym->num (get-and-remove 'x))
     (check-all-results '(#f 8) '(#f 8) '(#f 8) '(#f 5))
     
     (set! sym->num (get-and-remove 'y))
     (check-all-results '(#f #f) '(#f #f) '(#f #f) '(#f #f)))))

(define (run-alist-tests)
  (displayln "Running tests for alist.rkt")
  (run-tests tests))
