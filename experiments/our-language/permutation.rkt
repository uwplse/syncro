#lang rosette

(require "../src/rosette/namespace-requires.rkt"
         "../src/rosette/grammar/lifted-operators.rkt")

;; NOTE: Normally all except num2 would have mutable? set to #f, but we set it
;; to #t here to have a fair comparison with Bonsai
(define terminal-info (new Lexical-Terminal-Info%))
(send terminal-info make-and-add-terminal 'inverse-permutation
      (Vector-type 5 (Integer-type))
      #:mutable? #t)
(send terminal-info make-and-add-terminal 'permutation
      (Vector-type 5 (Integer-type))
      #:mutable? #t)
(send terminal-info make-and-add-terminal 'i (Integer-type) #:mutable? #t)
(send terminal-info make-and-add-terminal 'j (Integer-type) #:mutable? #t)


(displayln "Time for symbolic program generation:")
(define program
  (time
   (grammar terminal-info 2 2
            #:num-temps 0 #:guard-depth 0 #:type (Void-type)
            #:operators (list vector-set!^ vector-ref^ +^ *^ -^)
            #:version 'caching #:choice-version 'basic
            #:mode 'stmt #:print-statistics #f)))

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
    (for ([perm perms] [initinv init-inv-perms] [resinv res-inv-perms]
          [i i-list] [j j-list])
      (define initial-env
        (environment-define
         (environment-define
          (environment-define
           (environment-define
            global-environment
            'inverse-permutation
            initinv)
           'permutation
           perm)
          'i
          i)
         'j
         j))

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
    (displayln "No program found"))
