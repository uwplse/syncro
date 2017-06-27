#lang rosette

(require "env.rkt" "../enum-set.rkt" "../graph.rkt" "../map.rkt"
         "../operators.rkt" "../types.rkt" "language.rkt")

(provide
 global-environment
 ;; Operators that construct lifted AST nodes
 void^ not^ and^ or^ =^ <^ equal?^ +^ -^ *^ #;/^
 vector-increment!^ vector-decrement!^ vector-set!^ vector-ref^
 enum-set-add!^ enum-set-remove!^ enum-set-contains?^
 map-ref^ map-set!^
 add-edge!^ remove-edge!^ has-edge?^ vertex-parents^ vertex-children^)

;;;;;;;;;;;;;;;;;;;;;;;
;; Lifting operators ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Add and, or, not

(define alpha-any (Type-var))
(define alpha2-any (Type-var))
(define alpha-idx (Type-var (Index-type)))
(define bool (Boolean-type))
(define int (Integer-type))


(define void-type (Procedure-type '() (Void-type)))
(define not-type (Procedure-type (list bool) bool))
(define and-or-type (Procedure-type (list bool bool) bool))
(define cmp-type (Procedure-type (list int int) bool))
(define equal?-type (Procedure-type (list alpha-any alpha-any) bool))
(define arith-type (Procedure-type (list int int) int))


(define vec-inc/dec-type
  (Procedure-type (list (Vector-type alpha-idx int) alpha-idx)
                  (Void-type) #:write-index 0))
(define vec-set!-type
  (Procedure-type (list (Vector-type alpha-idx alpha-any) alpha-idx alpha-any)
                  (Void-type) #:write-index 0))
(define vec-ref-type
  (Procedure-type (list (Vector-type alpha-idx alpha-any) alpha-idx)
                  alpha-any #:read-index 0))


(define enum-set-modify-type
  (Procedure-type (list (Set-type alpha-any) alpha-any)
                  (Void-type) #:write-index 0))
(define enum-set-contains?-type
  (Procedure-type (list (Set-type alpha-any) alpha-any)
                  bool #:read-index 0))


(define map-ref-type
  (Procedure-type (list (Map-type 'unknown alpha-any alpha2-any)
                        alpha-any)
                  alpha2-any #:read-index 0))
(define map-set!-type
  (Procedure-type (list (Map-type 'unknown alpha-any alpha2-any)
                        alpha-any alpha2-any)
                  (Void-type) #:write-index 0))


(define graph-modify-type
  (Procedure-type (list (DAG-type alpha-any) alpha-any alpha-any)
                  (Void-type) #:write-index 0))
(define graph-has-edge?-type
  (Procedure-type (list (DAG-type alpha-any) alpha-any alpha-any)
                  bool #:read-index 0))
;; vertex-parent and vertex-children
;; Not a Read procedure because you are not allowed to mutate the return value.
(define graph-get-set-type
  (Procedure-type (list (DAG-type alpha-any) alpha-any)
                  (Set-type alpha-any)))

(define global-environment (make-environment))

(define-lifted
  global-environment
  [void void^ void-type] [not not^ not-type] ;; and/or defined below
  [= =^ cmp-type] [< <^ cmp-type] [equal? equal?^ equal?-type]
  [+ +^ arith-type] [- -^ arith-type] [* *^ arith-type] #;[/ /^ arith-type]

  [vector-increment! vector-increment!^ vec-inc/dec-type]
  [vector-decrement! vector-decrement!^ vec-inc/dec-type]
  [vector-set!       vector-set!^       vec-set!-type]
  [vector-ref        vector-ref^        vec-ref-type]

  [enum-set-add!      enum-set-add!^      enum-set-modify-type]
  [enum-set-remove!   enum-set-remove!^   enum-set-modify-type]
  [enum-set-contains? enum-set-contains?^ enum-set-contains?-type]

  [map-ref map-ref^ map-ref-type] [map-set! map-set!^ map-set!-type]

  [add-edge! add-edge!^ graph-modify-type]
  [remove-edge! remove-edge!^ graph-modify-type]
  [has-edge? has-edge?^ graph-has-edge?-type]
  [vertex-parents vertex-parents^ graph-get-set-type]
  [vertex-children vertex-children^ graph-get-set-type])

(define-lifted-using-proc
  global-environment
  [(lambda (x y) (and x y)) and and^ and-or-type]
  [(lambda (x y) (or x y))  or  or^  and-or-type])
