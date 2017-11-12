#lang rosette

(require "../../src/rosette/namespace-requires.rkt"
         "../../src/rosette/grammar/grammar-operators.rkt"
         "../../src/rosette/grammar/lifted-operators.rkt"
         "../../src/racket/cmd-parse.rkt")

(provide main)

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
  (send terminal-info make-and-add-terminal 'r1
        (Vector-type 2 (List-type 3 (Boolean-type)))
        #:mutable? #t)
  (send terminal-info make-and-add-terminal 'ψ
        (Vector-type 2 (Boolean-type))
        #:mutable? always-mutable?)
  (send terminal-info make-and-add-terminal 'i
        (Integer-type)
        #:mutable? always-mutable?)
  (send terminal-info make-and-add-terminal 'formula
        (Boolean-type)
        #:mutable? always-mutable?)

  (displayln "Time for symbolic program generation")
  (define program
    (time
     (grammar terminal-info 2 3
              #:num-temps 0 #:guard-depth 1 #:type (Void-type)
              #:operators (list vector-set!^ vector-ref^
                                grm-if^ void^)
              #:version (hash-ref options 'grammar-version)
              #:choice-version (hash-ref options 'grammar-choice)
              #:cache? (hash-ref options 'cache?)
              #:disable-types? (hash-ref options 'no-type-analysis?)
              #:mode 'stmt #:print-statistics #t)))

  (displayln "Synthesizing update rule for ψ from examples")
  (define input-output-examples
    (list (list (vector '(#f #f) '(#t)) 0 #t (vector #t #f) (vector #f #f))
          (list (vector '(#f #t #t) '(#f)) 1 #t (vector #f #t) (vector #f #f))
          (list (vector '(#t #t #f) '(#t)) 1 #t (vector #f #f) (vector #f #f))
          (list (vector '(#f #f #f) '(#f)) 0 #f (vector #t #t) (vector #t #t))
          (list (vector '(#f #f #f) '(#f)) 1 #f (vector #t #t) (vector #t #t))))

  (define synth
    (time
     (solve
      (for ([parameters input-output-examples])
        (match-define (list r1 i form old-ψ new-ψ)
            parameters)
        (define initial-env
          (extend-environment global-environment r1 old-ψ i form))

        (define final-env (second (eval-lifted program initial-env)))
        
        (define result
          (first
           (eval-lifted
            (send terminal-info get-terminal-by-id 'ψ)
            final-env)))
        (assert (equal? result new-ψ))))))

  (if (sat? synth)
      (let-values ([(_ cleaned-code)
                    (eliminate-dead-code '() (coerce-evaluate program synth))])
        (pretty-print (lifted-code cleaned-code)))
      (displayln "No program found")))
