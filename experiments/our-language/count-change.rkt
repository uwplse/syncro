#lang rosette

(require "debug.rkt"
         "../../src/rosette/namespace-requires.rkt"
         "../../src/rosette/grammar/grammar-operators.rkt"
         "../../src/rosette/grammar/lifted-operators.rkt"
         "../../src/racket/cmd-parse.rkt")

(provide main)

(current-bitwidth 5)

(define (main . args)
  (define options (cmd-parse args))
  (printf "Using grammar ~a with choice ~a ~a caching~%"
          (hash-ref options 'grammar-version)
          (hash-ref options 'grammar-choice)
          (if (hash-ref options 'cache?) "with" "without"))
  (define always-mutable? (hash-ref options 'no-mutability-analysis?))
  (when always-mutable?
    (displayln "Turning off the mutability analysis"))
  (when (hash-ref options 'no-type-analysis?)
    (displayln "Turning off the type analysis"))

  (define terminal-info (new Lexical-Terminal-Info%))
  (send terminal-info make-and-add-terminal 'cache
        (Vector-type (Integer-type) (Vector-type (Integer-type) (Integer-type)))
        #:mutable? #t)
  (send terminal-info make-and-add-terminal 'amount (Integer-type)
        #:mutable? always-mutable?)
  (send terminal-info make-and-add-terminal 'coin-num (Integer-type)
        #:mutable? always-mutable?)
  (send terminal-info make-and-add-terminal 'coins
        (Vector-type (Integer-type) (Integer-type))
        #:mutable? always-mutable?)
  (send terminal-info make-and-add-terminal 'zero (Integer-type)
        #:mutable? always-mutable?)
  (send terminal-info make-and-add-terminal 'one (Integer-type)
        #:mutable? always-mutable?)

  (define (vector-ref-default vec i default)
    (if (or (< i 0) (>= i (vector-length vec)))
        default
        (vector-ref vec i)))
  (define (make-matrix num-rows num-cols default)
    (build-vector num-rows (lambda (i) (make-vector num-cols default))))
  (define (matrix-ref matrix i j [default #f])
    (if default
        (vector-ref-default (vector-ref-default matrix i (vector)) j default)
        (vector-ref (vector-ref matrix i) j)))
  (define (matrix-set! matrix i j val)
    (vector-set! (vector-ref matrix i) j val))
  (define zero 0)
  (define one 1)

  (define (count-change coins total-amount)
    (define num-cache-coins (+ (vector-length coins) 1))
    (define num-cache-amounts (+ total-amount 1))
    (define cache (make-matrix num-cache-coins num-cache-amounts 0))

    ;; Base cases
    (for ([i num-cache-amounts])
      (matrix-set! cache (vector-length coins) i 0))
    (for ([j num-cache-coins])
      (matrix-set! cache j 0 1))
    ;; Recursive case
    (for* ([coin-num (range (- (vector-length coins) 1) -1 -1)]
           [amount (range 1 num-cache-amounts)])
      (define env
        (extend-environment global-environment cache amount coin-num coins zero one))
      (matrix-set! cache coin-num amount
                   (first (eval-lifted program env))
                   #;(+ (matrix-ref cache coin-num (- amount (vector-ref coins coin-num)) 0)
                      (matrix-ref cache (+ coin-num 1) amount))))

    ;(for ([row cache]) (displayln row)) (newline)
    (matrix-ref cache 0 total-amount))

  (displayln "Time for symbolic program generation:")
  (define program
    (time
     (grammar terminal-info (hash-ref options 'stmts) (hash-ref options 'depth)
              #:num-temps 0 #:guard-depth 0 #:type (Integer-type)
              #:operators (list vector-set!^ vector-ref^ vector-ref-default^ +^ -^)
              #:version (hash-ref options 'grammar-version)
              #:choice-version (hash-ref options 'grammar-choice)
              #:cache? (hash-ref options 'cache?)
              #:disable-types? (hash-ref options 'no-type-analysis?)
              #:use-constants? #f
              #:mode 'stmt #:print-statistics #t)))

  (displayln "Time for synthesis by examples")
  (define coin-vecs
    (list (vector 1 2)
          (vector 1 2)
          (vector 1 2 3)
          (vector 2 3)
          (vector 1 3 5)
          (vector 1 3 5)
          (vector 1 2 5)
          (vector 2 5)))
  (define amounts
    (list 5 6 6 7 10 11 10 10))
  (define results
    (list 3 4 7 1 7 8 10 2))

  #;(for ([coins coin-vecs] [amount amounts] [result results])
    (printf "(cc ~a ~a) = ~a instead of ~a~%"
            coins amount (count-change coins amount) result))

  (define synth
    (time
     (solve
      (begin
        (define constraints (make-hash))
        #;(assert (wild-equal? (lifted-code program)
                             '(let ()
                                (define (wild 2) (vector-ref cache coin-num))
                                (define (wild 3) (+ coin-num (wild)))
                                (define (wild 4) (vector-ref cache (wild)))
                                (define (wild 5) (vector-ref coins coin-num))
                                (define (wild 6) (wild))
                                (define (wild 7) (- amount (wild)))
                                (+ (vector-ref (wild) amount)
                                   (vector-ref-default (wild) (wild) (wild))))
                             constraints))
        #;(for ([constraint (hash-values constraints)])
          (for ([elem1 constraint] [elem2 (cdr constraint)])
            (assert (equal? elem1 elem2))))
        (for ([coins coin-vecs] [amount amounts] [result results])
          (assert (equal? (count-change coins amount) result)))))))

  (if (sat? synth)
      (let-values ([(_ cleaned-code)
                    (eliminate-dead-code '() (coerce-evaluate program synth))])
        (pretty-print (lifted-code (coerce-evaluate program synth)))
        (pretty-print (lifted-code cleaned-code)))
      (displayln "No program found")))
