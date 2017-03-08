#lang rosette

(require "../enum-set.rkt" "../graph.rkt" "../operators.rkt" "../types.rkt"
          "language.rkt")

(provide operator-info extra-operators (struct-out special-form)
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
(define graph-modify-type
  (Procedure-type (list (DAG-type alpha-any) alpha-any alpha-any)
                  (Void-type) #:write-index 0))
(define graph-has-edge?-type
  (Procedure-type (list (DAG-type alpha-any) alpha-any alpha-any)
                  (Boolean-type) #:read-index 0))
;; vertex-parent and vertex-children
;; Not a Read procedure because you are not allowed to mutate the return value.
(define graph-get-set-type
  (Procedure-type (list (DAG-type alpha-any) alpha-any)
                  (Set-type alpha-any)))

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
  [add-edge! add-edge!^ graph-modify-type]
  [remove-edge! remove-edge!^ graph-modify-type]
  [has-edge? has-edge?^ graph-has-edge?-type]
  [vertex-parents vertex-parents^ graph-get-set-type]
  [vertex-children vertex-children^ graph-get-set-type]
  [enum-set-add! enum-set-add!^ enum-set-modify-type]
  [enum-set-remove! enum-set-remove!^ enum-set-modify-type]
  [enum-set-contains? enum-set-contains?^ enum-set-contains?-type]
  [equal? equal?^ (Procedure-type (list alpha-any alpha-any) (Boolean-type))]
  [= =^ cmp-type] [< <^ cmp-type]
  [+ +^ arith-type] [- -^ arith-type] [* *^ arith-type] #;[/ /^ arith-type])

(display "add-edge: ") (displayln add-edge!^)

(struct special-form (name constructor) #:transparent)
(define operator-info
  (list void^ vector-increment!^ vector-decrement!^ vector-set!^ vector-ref^
        enum-set-add!^ enum-set-remove!^ enum-set-contains?^
        add-edge!^ remove-edge!^ has-edge?^
        equal?^ =^ <^ +^ -^ *^
        (special-form 'if if^) (special-form 'set! set!^)))

;; Non constant time operators that can't be used in grammars but can
;; be used in sketches
(define extra-operators
  (list vertex-parents^ vertex-children^))
