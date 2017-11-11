#lang rosette

(require "../../src/rosette/namespace-requires.rkt"
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

  ;; NOTE: Normally all except num2 would have mutable? set to #f, but we set it
  ;; to #t here to have a fair comparison with Bonsai
  (define terminal-info (new Lexical-Terminal-Info%))
  (send terminal-info make-and-add-terminal 'inverse-permutation
        (Vector-type 5 (Integer-type))
        #:mutable? #t)
  (send terminal-info make-and-add-terminal 'permutation
        (Vector-type 5 (Integer-type))
        #:mutable? always-mutable?)
  (send terminal-info make-and-add-terminal 'i (Integer-type) #:mutable? always-mutable?)
  (send terminal-info make-and-add-terminal 'j (Integer-type) #:mutable? always-mutable?)


  (displayln "Time for symbolic program generation:")
  (define program
    (time
     (grammar terminal-info 2 2
              #:num-temps 0 #:guard-depth 0 #:type (Void-type)
              #:operators (list vector-set!^ vector-ref^ +^ *^ -^)
              #:version (hash-ref options 'grammar-version)
              #:choice-version (hash-ref options 'grammar-choice)
              #:cache? (hash-ref options 'cache?)
              #:disable-types? (hash-ref options 'no-type-analysis?)
              #:mode 'stmt #:print-statistics #t)))
  ;(displayln program)

  (displayln "Time for synthesis by examples")
  (define perms
    (list (vector 0 1 3 4 2) (vector 1 2 0 3 4) (vector 0 4 1 2 3)
          (vector 4 1 2 0 3) (vector 4 3 1 2 0)))
  (define init-inv-perms
    (list (vector 0 4 1 2 3) (vector 2 4 1 3 0) (vector 1 2 3 4 0)
          (vector 1 3 2 4 0) (vector 4 3 2 1 0)))
  (define res-inv-perms
    (list (vector 0 1 4 2 3) (vector 2 0 1 3 4) (vector 0 2 3 4 1)
          (vector 3 1 2 4 0) (vector 4 2 3 1 0)))
  (define i-list (list 4 0 1 3 2))
  (define j-list (list 1 4 0 1 3))

  (define synth
    (time
     (solve
      (for ([permutation perms]
            [inverse-permutation init-inv-perms]
            [resinv res-inv-perms]
            [i i-list]
            [j j-list])
        ;; In order to use the extend-environment macro, the names of the
        ;; variables must match the names in the Lexical-Terminal-Info% object.
        (define initial-env
          (extend-environment global-environment inverse-permutation permutation i j))

        (define final-env (second (eval-lifted program initial-env)))
        
        (define result
          (first
           (eval-lifted
            (send terminal-info get-terminal-by-id 'inverse-permutation)
            final-env)))
        (assert (equal? result resinv))))))

  (if (sat? synth)
      (let-values ([(_ cleaned-code)
                    (eliminate-dead-code '() (coerce-evaluate program synth))])
        (pretty-print (lifted-code cleaned-code)))
      (displayln "No program found")))
