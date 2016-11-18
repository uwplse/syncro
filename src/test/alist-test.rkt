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

     (define (check-get-and-remove! key value)
       (check-true (alist-has-key? sym->num key))
       (define res (alist-get-and-remove! sym->num key))
       (if (list? value)
           (check-not-false (member res value))
           (check-equal? res value)))

     (define (check-not-present key)
       (check-false (alist-has-key? sym->num key))
       (check-exn exn:fail?
                  (thunk (alist-get sym->num key)))
       (check-exn exn:fail?
                  (thunk (alist-get-and-remove! sym->num key))))
     
     (alist-insert! sym->num 'x 3)
     (check-get 'x 3)
     (check-not-present 'foo)
     
     (alist-insert! sym->num 'x 6)
     (alist-insert! sym->num 'foo 5)
     (alist-insert! sym->num 'x 9)
     (alist-insert! sym->num 'y 3)
     (check-get 'foo 5)
     ;; Get with x can return any of 3, 6, and 9
     (check-get 'x '(3 6 9))
     (check-get 'y 3)
     
     (check-get-and-remove! 'foo 5)
     (check-not-present 'foo)
     ;; Remove all copies of x
     (for ([i 3])
       (check-get-and-remove! 'x '(3 6 9)))
     (check-not-present 'x)
     (alist-insert! sym->num 'x 10)
     (check-get 'x 10)

     ;; Remove everything
     (check-get-and-remove! 'y 3)
     (check-not-present 'y)
     (check-get-and-remove! 'x 10)
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

     (define (get-and-remove! key)
       (if (alist-has-key? sym->num key)
           (alist-get-and-remove! sym->num key)
           sym->num))
     
     (if b1
         (alist-insert! sym->num 'x 3)
         (alist-insert! sym->num (if b2 'x 'y) 5))
     (check-all-results '(3 #f) '(3 #f) '(5 #f) '(#f 5))
     
     (when b1
       (get-and-remove! 'x))
     (check-all-results '(#f #f) '(#f #f) '(5 #f) '(#f 5))

     ;; Here we assume that when inserting multiple values with the
     ;; same key into an alist, it behaves like a stack. This is not
     ;; actually required by the interface but matches the current
     ;; semantics.
     (if (and b1 b2)
         (alist-insert! sym->num 'x 6)
         (alist-insert! sym->num 'x 7))
     (check-all-results '(6 #f) '(7 #f) '(7 #f) '(7 5))

     (when (or b1 b2)
       (alist-insert! sym->num 'y 8))
     (check-all-results '(6 8) '(7 8) '(7 8) '(7 5))

     (unless b1
       (get-and-remove! 'x))
     (check-all-results '(6 8) '(7 8) '(5 8) '(#f 5))
     
     (get-and-remove! 'x)
     (check-all-results '(#f 8) '(#f 8) '(#f 8) '(#f 5))
     
     (get-and-remove! 'y)
     (check-all-results '(#f #f) '(#f #f) '(#f #f) '(#f #f)))))

(define (run-alist-tests)
  (displayln "Running tests for alist.rkt")
  ;; Internal errors print out the error messages to stdout, which we
  ;; don't care about, suppress it.
  (define port (open-output-string))
  (define result
    (parameterize ([current-output-port port])
      (run-tests tests)))
  (close-output-port port)
  result)
