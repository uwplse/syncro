#lang rosette

(require "../enum-set.rkt" "../operators.rkt" "language.rkt" "../types.rkt")

(provide void^ vector-increment!^ vector-decrement!^ vector-set!^ vector-ref^
         enum-set-add!^ enum-set-remove!^ enum-set-contains?^
         equal?^ =^ <^ +^ -^ *^ #;/^)

;;;;;;;;;;;;;;;;;;;;;;;
;; Lifting operators ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Add and, or, not

(match-define (list alpha-v1 alpha-v2 alpha-v3 alpha-v4)
  (build-list 4 (lambda (i) (Type-var (Index-type)))))
(match-define (list beta-v3 beta-v4 alpha-v5 alpha-v6 alpha-v7 alpha-v8)
  (build-list 6 (lambda (i) (Type-var))))

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
  [enum-set-add!
   enum-set-add!^
   (Procedure-type (list (Set-type alpha-v5) alpha-v5)
                   (Void-type) #:write-index 0)]
  [enum-set-remove!
   enum-set-remove!^
   (Procedure-type (list (Set-type alpha-v6) alpha-v6)
                   (Void-type) #:write-index 0)]
  [enum-set-contains?
   enum-set-contains?^
   (Procedure-type (list (Set-type alpha-v7) alpha-v7) (Boolean-type))]
  [equal? equal?^ (Procedure-type (list alpha-v8 alpha-v8) (Boolean-type))]
  [= =^ cmp-type] [< <^ cmp-type]
  [+ +^ arith-type] [- -^ arith-type] [* *^ arith-type] #;[/ /^ arith-type])
