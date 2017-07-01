#lang rosette

(require rackunit rackunit/text-ui)
(require "../rosette/grammar/lifted-operators.rkt"
         "../rosette/grammar/language.rkt"
         "../rosette/grammar/env.rkt"
         "../rosette/enum-set.rkt"
         "../rosette/types.rkt")

(provide run-language-tests)

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))

(define tests
  (test-suite
   "Tests for language.rkt"
   (let* ([env (make-environment
                (list (cons '+ +) (cons '- -) (cons '* *)
                      (cons '< <) (cons '= =)
                      (cons 'void void)
                      (cons 'vector-set! vector-set!)
                      (cons 'evens (build-enum-set 10 even?))
                      (cons 'my-vec (make-vector 10 -1))))]
          [x (make-lifted-variable 'x (Integer-type))]
          [y (make-lifted-variable 'y (Integer-type))]
          [digit-type (Enum-type 'Digit 10)]
          [digit-var (make-lifted-variable 'digit digit-type)]
          [evens (make-lifted-variable 'evens (Set-type digit-type))]
          [my-vec (make-lifted-variable 'my-vec (Vector-type 10 digit-type))])

     (define (lifted-test obj expected-result [env env])
       (match-define (list result _) (eval-lifted obj env))
       (check-equal?
        result expected-result
        (format "Code ~a gave result ~a instead of the expected ~a"
                (lifted-code obj) result expected-result)))

     (test-case "eval-lifted and lifted-code invariant"
       (define lifted-objects
         (list (+^ 2 3)
               (if^ (=^ 3 (+^ 1 2))
                    (begin^ (void^) (*^ 9 2))
                    (void^))))
       
       (for ([obj lifted-objects])
         (lifted-test obj (eval (lifted-code obj) ns))))

     (test-case "environment manipulations"
       (lifted-test (begin^ (set!^ +^ *^) (+^ 3 5)) 15)
       (lifted-test (begin^ (define^ x (*^ 2 5)) (+^ x x)) 20)
       (lifted-test (begin^ (define^ x 10)
                            (define^ y 3)
                            (if^ (<^ x 5)
                                 (+^ x y)
                                 (begin^ (set!^ x y)
                                         (set!^ y x) ;; guaranteed nop
                                         ((if^ (<^ x 5) *^ +^) x y))))
                    9))

     (test-case "for loops"
       (lifted-test (begin^ (for-enum-set^ digit-var evens
                              (vector-set!^ my-vec digit-var digit-var))
                            my-vec)
                    (vector 0 -1 2 -1 4 -1 6 -1 8 -1)))

     (test-case "symbolic examples"
       (define-symbolic* b1 b2 boolean?)
       (define (check-all actual expected)
         (for ([formula (list (and      b1  b2) (and      b1  (not b2))
                              (and (not b1) b2) (and (not b1) (not b2)))]
               [result expected])
           (let* ([model (solve (assert formula))]
                  [eval-actual (evaluate actual model)])
             (check-equal? eval-actual result
                           (format "Got ~a, expected ~a for condition ~a"
                                   eval-actual result formula)))))

       (define program
         (begin^ (define^ x (if (and b1 b2) 10 4))
                 (define^ y 3)
                 (if^ (if b1 (<^ x 5) (=^ x y))
                      (begin^ (set!^ y (+^ x y)) (-^ y x))
                      (begin^ (set!^ (if (not b2) x y) 6)
                              (set!^ x y)
                              (set!^ y x) ;; guaranteed nop
                              ((if^ (<^ x 5) *^ +^) x y)))))
       (match-define (list result final-env)
         (eval-lifted program env))
       (check-all result '(12 3 12 9))
       (check-all (environment-ref final-env 'x) '(6 4 6 3))
       (check-all (environment-ref final-env 'y) '(6 7 6 3)))

     (test-case "specific behavior"
       ;; Specific behaviors in the current implementation that may
       ;; change in the future. These tests make sure we notice if the
       ;; behavior changes.
       (lifted-test (*^ (+^ (begin^ (define^ x 5) 1) x)
                        x)
                    30)))))

(define (run-language-tests)
  (displayln "Running tests for language.rkt")
  (run-tests tests))

(module+ main
  (run-language-tests))
