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

  ;; Characters will be labelled 1 - 26, 0 is unused
  (define Character (Enum-type 'Character 27))
  (define terminal-info (new Lexical-Terminal-Info%))
  (send terminal-info make-and-add-terminal 'cache
        (Vector-type (Integer-type) (Vector-type (Integer-type) (Integer-type)))
        #:mutable? #t)
  (send terminal-info make-and-add-terminal 's1
        (Vector-type (Integer-type) Character)
        #:mutable? always-mutable?)
  (send terminal-info make-and-add-terminal 's2
        (Vector-type (Integer-type) Character)
        #:mutable? always-mutable?)
  (send terminal-info make-and-add-terminal 'i (Integer-type) #:mutable? always-mutable?)
  (send terminal-info make-and-add-terminal 'j (Integer-type) #:mutable? always-mutable?)

  (define (make-matrix num-rows num-cols default)
    (build-vector num-rows (lambda (i) (make-vector num-cols default))))
  (define (matrix-ref matrix i j)
    (vector-ref (vector-ref matrix i) j))
  (define (matrix-set! matrix i j val)
    (vector-set! (vector-ref matrix i) j val))

  (define (edit-distance s1 s2)
    (define s1chars (+ 1 (vector-length s1)))
    (define s2chars (+ 1 (vector-length s2)))
    (define cache (make-matrix s1chars s2chars 0))

    ;; Base cases
    (for ([i s1chars])
      (matrix-set! cache i 0 i))
    (for ([j s2chars])
      (matrix-set! cache 0 j j))
    ;; Recursive case
    (for* ([i (vector-length s1)]
           [j (vector-length s2)])
      (define env
        (extend-environment global-environment cache s1 s2 i j))
      (matrix-set! cache (+ i 1) (+ j 1)
                   (first (eval-lifted program env))
                   #;(min (+ 1 (matrix-ref cache i (+ j 1)))
                        (+ 1 (matrix-ref cache (+ i 1) j))
                        (+ (if (equal? (vector-ref s1 i) (vector-ref s2 j)) 0 1)
                           (matrix-ref cache i j)))))
    ;(for ([row cache]) (displayln row)) (newline)
    (matrix-ref cache (vector-length s1) (vector-length s2)))

  (displayln "Time for symbolic program generation:")
  (define program
    (time
     (grammar terminal-info (hash-ref options 'stmts) (hash-ref options 'depth)
              #:num-temps 0 #:guard-depth 0 #:type (Integer-type)
              #:operators (list vector-set!^ vector-ref^ +^ -^ min^ equal?^ if-expr^)
              #:version (hash-ref options 'grammar-version)
              #:choice-version (hash-ref options 'grammar-choice)
              #:cache? (hash-ref options 'cache?)
              #:disable-types? (hash-ref options 'no-type-analysis?)
              #:mode 'stmt #:print-statistics #t)))

  (displayln "Time for synthesis by examples")
  (define s1s
    (list (vector 1 2 3)
          (vector 2 3)

          (vector 1 2 3 2 1)
          (vector 1 3 1)

          (vector 1 2)
          (vector 1 3 1)

          (vector 1 2 3 2 1)
          (vector 1 1)

          (vector 1 2 3 4)

          (vector 1 2 3 4)
          (vector 5 6 7)))
  (define s2s
    (list (vector 2 3)
          (vector 1 2 3)

          (vector 1 3 1)
          (vector 1 2 3 2 1)

          (vector 1 3 1)
          (vector 1 2)

          (vector 1 1)
          (vector 1 2 3 2 1)

          (vector 2 5 4 5)

          (vector 5 6 7)
          (vector 1 2 3 4)))
  (define edit-distances
    (list 1 1 2 2 2 2 3 3 3 4 4))

  (define synth
    (time
     (solve
      (begin
        #;(define constraints (make-hash))
        #;(assert (wild-equal? (lifted-code program)
                             '(let ()
                                (define (wild 1) 0)
                                (define (wild 2) 1)
                                (define (wild 3) (vector-ref cache i))
                                (define (wild 4) (vector-ref s1 i))
                                (define (wild 5) (+ i (wild)))
                                (define (wild 6) #t)
                                (define (wild 7) (vector-ref cache (wild)))
                                (define (wild 8) (vector-ref s2 j))
                                (define (wild 9) (+ j (wild)))
                                (define (wild 10) (equal? (wild) (wild)))
                                (define (wild 11) (wild))
                                (define (wild 12) (wild))
                                (define (wild 13) (if (wild) (wild) (wild)))
                                (define (wild 14) #t)
                                (define (wild 15) (wild))
                                (define (wild 16) (wild))
                                (define (wild 17) (vector-ref (wild) j))
                                (define (wild 18) #t)
                                (define (wild 19) (wild))
                                (define (wild 20) (wild))
                                (define (wild 21) (- (wild) (wild)))
                                (define (wild 22) #t)
                                (define (wild 23) (wild))
                                (define (wild 24) (wild))
                                (define (wild 25) (vector-ref (wild) (wild)))
                                (define (wild 26) #t)
                                (define (wild 27) (wild))
                                (define (wild 28) (wild))
                                (define (wild 29) (vector-ref (wild) j))
                                (define (wild 30) #t)
                                (define (wild 31) (wild))
                                (define (wild 32) (wild))
                                (define (wild 33) (min (wild) (wild)))
                                (define (wild 34) #t)
                                (+ (wild 2) (min (wild) (wild))))
                             constraints))
        #;(for ([constraint (hash-values constraints)])
          (for ([elem1 constraint] [elem2 (cdr constraint)])
            (assert (equal? elem1 elem2))))
        (for ([s1 s1s] [s2 s2s] [dist edit-distances])
          (assert (equal? (edit-distance s1 s2) dist)))))))

  (if (sat? synth)
      (let-values ([(_ cleaned-code)
                    (eliminate-dead-code '() (coerce-evaluate program synth))])
        (pretty-print (lifted-code (coerce-evaluate program synth)))
        (pretty-print (lifted-code cleaned-code)))
      (displayln "No program found")))
