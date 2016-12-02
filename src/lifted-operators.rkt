#lang rosette

(require "operators.rkt" "rosette-util.rkt" "types.rkt")

(provide void^ vector-increment!^ vector-decrement!^ vector-set!^ vector-ref^
         equal?^ =^ <^ +^ -^ *^ #;/^)

;;;;;;;;;;;;;;;;;;;;;;;
;; Lifting operators ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Put all of this into its own file (can be used by rosette-util-test)
(match-define (list alpha-v1 alpha-v2 alpha-v3 alpha-v4)
  (build-list 4 (lambda (i) (Type-var (Index-type)))))
(match-define (list beta-v3 beta-v4 alpha-v5)
  (build-list 3 (lambda (i) (Type-var))))

(define cmp-type
  (Procedure-type (list (Integer-type) (Integer-type)) (Boolean-type)))
(define arith-type
  (Procedure-type (list (Integer-type) (Integer-type)) (Integer-type)))

(define-lifted
  [void void^ (Procedure-type '() (Void-type))]
  [vector-increment!
   vector-increment!^
   (Procedure-type (list (Vector-type alpha-v1 (Integer-type)) alpha-v1)
                   (Void-type) #:write-index 0)]
  [vector-decrement!
   vector-decrement!^
   (Procedure-type (list (Vector-type alpha-v2 (Integer-type)) alpha-v2)
                   (Void-type) #:write-index 0)]
  [vector-set!
   vector-set!^
   (Procedure-type (list (Vector-type alpha-v3 beta-v3) alpha-v3 beta-v3)
                   (Void-type) #:write-index 0)]
  [vector-ref
   vector-ref^
   (Procedure-type (list (Vector-type alpha-v4 beta-v4) alpha-v4)
                   beta-v4 #:read-index 0)]
  [equal? equal?^ (Procedure-type (list alpha-v5 alpha-v5) (Boolean-type))]
  [= =^ cmp-type] [< <^ cmp-type]
  [+ +^ arith-type] [- -^ arith-type] [* *^ arith-type] #;[/ /^ arith-type])
