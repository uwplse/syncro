#lang rosette

(require "../../src/rosette/namespace-requires.rkt"
         "../../src/rosette/grammar/grammar-operators.rkt"
         "../../src/rosette/grammar/lifted-operators.rkt"
         "../../src/racket/cmd-parse.rkt")

(provide main)

;; NOTE: To have a fair comparison with Bonsai, we should make all of
;; these have mutable? #f (except num2), but if we do this, then the
;; synthesized code will mutate num2helper incorrectly because we
;; don't tell it not to do that.
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
  (send terminal-info make-and-add-terminal 'num2
        (Vector-type (Enum-type 'Document 2) (Integer-type))
        #:mutable? #t)
  (send terminal-info make-and-add-terminal 'num2helper
        (Vector-type
         (Enum-type 'Document 2)
         (Vector-type (Enum-type 'Topic 3) (Integer-type)))
        #:mutable? always-mutable?)
  ;; The word isn't actually needed here.
  (send terminal-info make-and-add-terminal 'word
        (Enum-type 'Word 12)
        #:mutable? always-mutable?)
  (send terminal-info make-and-add-terminal 'old-topic
        (Enum-type 'Topic 3)
        #:mutable? always-mutable?)
  (send terminal-info make-and-add-terminal 'new-topic
        (Enum-type 'Topic 3)
        #:mutable? always-mutable?)
  (send terminal-info make-and-add-terminal 'word->document
        (Vector-type
         (Enum-type 'Word 12)
         (Enum-type 'Document 2))
        #:mutable? always-mutable?)

  (displayln "Time for symbolic program generation")
  (define program
    (time
     (grammar terminal-info 2 3
              #:num-temps 0 #:guard-depth 1 #:type (Void-type)
              #:operators (list vector-increment!^ vector-decrement!^ vector-ref^
                                grm-if^ void^ +^ *^ -^ =^)
              #:version (hash-ref options 'grammar-version)
              #:choice-version (hash-ref options 'grammar-choice)
              #:cache? (hash-ref options 'cache?)
              #:disable-types? (hash-ref options 'no-type-analysis?)
              #:mode 'stmt #:print-statistics #t)))

  (define (matrixify list-of-lists)
    (list->vector (map list->vector list-of-lists)))

  (displayln "Synthesizing update rule for num2 from examples")
  ;; First five words are document 0, remaining seven are document 1
  (define word->document (vector 0 0 0 0 0 1 1 1 1 1 1 1))
  ;; Each input is of the form num2helper, word, old-topic, new-topic, old num2, expected new num2
  (define input-output-examples
    (list (list (matrixify '((2 2 1) (5 2 0))) 3 0 1 (vector 3 2) (vector 3 2))
          (list (matrixify '((2 2 1) (5 2 0))) 1 2 1 (vector 3 2) (vector 2 2))
          (list (matrixify '((2 2 1) (5 2 0))) 9 1 0 (vector 3 2) (vector 3 2))
          (list (matrixify '((2 2 1) (5 2 0))) 7 0 2 (vector 3 2) (vector 3 3))
          (list (matrixify '((0 1 4) (5 2 0))) 0 1 0 (vector 2 2) (vector 2 2))
          (list (matrixify '((0 1 4) (5 2 0))) 4 1 2 (vector 2 2) (vector 1 2))
          (list (matrixify '((0 1 4) (5 2 0))) 8 0 2 (vector 2 2) (vector 2 3))))

  (define synth
    (time
     (solve
      (for ([parameters input-output-examples])
        (match-define (list num2helper word old-topic new-topic num2 new-num2)
          parameters)
        (define initial-env
          (extend-environment global-environment num2 num2helper word old-topic new-topic word->document))

        (define final-env (second (eval-lifted program initial-env)))
        
        (define result
          (first
           (eval-lifted
            (send terminal-info get-terminal-by-id 'num2)
            final-env)))
        (assert (equal? result new-num2))))))

  (if (sat? synth)
      (let-values ([(_ cleaned-code)
                    (eliminate-dead-code '() (coerce-evaluate program synth))])
        (pretty-print (lifted-code cleaned-code)))
      (displayln "No program found")))
