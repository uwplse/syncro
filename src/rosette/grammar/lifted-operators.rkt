#lang rosette

(require "../enum-set.rkt" "../graph.rkt" "../operators.rkt" "../types.rkt"
          "language.rkt")

(provide operator-info (struct-out special-form)
         void^ vector-increment!^ vector-decrement!^ vector-set!^ vector-ref^
         enum-set-add!^ enum-set-remove!^ enum-set-contains?^
         equal?^ =^ <^ +^ -^ *^ #;/^)

;;;;;;;;;;;;;;;;;;;;;;;
;; Lifting operators ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Add and, or, not

(define alpha-any (Type-var))
(define alpha-idx (Type-var (Index-type)))

(define cmp-type
  (Procedure-type (list (Integer-type) (Integer-type)) (Boolean-type)))
(define arith-type
  (Procedure-type (list (Integer-type) (Integer-type)) (Integer-type)))
(define vec-inc/dec-type
  (Procedure-type (list (Vector-type alpha-idx (Integer-type)) alpha-idx)
                  (Void-type) #:write-index 0))
(define enum-set-modify-type
  (Procedure-type (list (Set-type alpha-any) alpha-any)
                  (Void-type) #:write-index 0))
(define enum-set-contains?-type
  (Procedure-type (list (Set-type alpha-any) alpha-any)
                  (Boolean-type) #:read-index 0))

(define-lifted
  [void void^ (Procedure-type '() (Void-type))]
  [vector-increment! vector-increment!^ vec-inc/dec-type]
  [vector-decrement! vector-decrement!^ vec-inc/dec-type]
  [vector-set!
   vector-set!^
   (Procedure-type (list (Vector-type alpha-idx alpha-any) alpha-idx alpha-any)
                   (Void-type) #:write-index 0)]
  [vector-ref
   vector-ref^
   (Procedure-type (list (Vector-type alpha-idx alpha-any) alpha-idx)
                   alpha-any #:read-index 0)]
  [has-edge?
   has-edge?^
   (Procedure-type (list (DAG-type alpha-any) alpha-any alpha-any)
                   (Boolean-type) #:read-index 0)]
  [enum-set-add! enum-set-add!^ enum-set-modify-type]
  [enum-set-remove! enum-set-remove!^ enum-set-modify-type]
  [enum-set-contains? enum-set-contains?^ enum-set-contains?-type]
  [equal? equal?^ (Procedure-type (list alpha-any alpha-any) (Boolean-type))]
  [= =^ cmp-type] [< <^ cmp-type]
  [+ +^ arith-type] [- -^ arith-type] [* *^ arith-type] #;[/ /^ arith-type])


(struct special-form (name constructor) #:transparent)
(define operator-info
  (list void^ vector-increment!^ vector-decrement!^ vector-set!^ vector-ref^
        enum-set-add!^ enum-set-remove!^ enum-set-contains?^ has-edge?^
        equal?^ =^ <^ +^ -^ *^
        (special-form 'if if^) (special-form 'set! set!^)))
