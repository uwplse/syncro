#lang rosette

(require "lifted-operators.rkt" "language.rkt" "../types.rkt" "../variable.rkt")

(require rosette/lib/angelic rosette/lib/synthax)

(provide grammar-synthax-deep22 grammar-synthax-deep33 grammar-synthax-deep34)

(define (grammar-synthax-deep22 info)
  ;; Using this grammar leads to long compilation times, so we disable
  ;; it when not using it.
  (error "grammar-synthax-deep22 has been disabled")
  ;(begin^ (stmt info 2 2))
)

(define (grammar-synthax-deep33 info)
  (error "grammar-synthax-deep33 has been disabled")
  ;(begin^ (stmt info 3 3))
)

(define (grammar-synthax-deep34 info)
  (error "grammar-synthax-deep34 has been disabled")
  ;(begin^ (stmt info 3 4))
)

(define-synthax terminals
  ([(_ terminal-info desired-type mutable?)
    (apply choose* (send terminal-info get-terminals
                         #:type desired-type
                         #:mutable? mutable?))]))

(define-synthax (stmt info depth num-stmts)
  #:base (void^)
  #:else (choose (void^)
                 (begin^ (base-stmt info depth num-stmts)
                         (stmt info depth (- num-stmts 1)))))

;; Vector statements, set statements, set!, and if
(define-synthax base-stmt
  ([(_ info depth num-stmts)
    (choose (vector-stmt info depth)
            (set-stmt info depth)
            (set!-stmt info depth)
            (if^ (expr info (Boolean-type) #:mutable? #f depth)
                 (stmt info depth (min (- num-stmts 1) 1))
                 (stmt info depth (min (- num-stmts 1) 1))))]))

;; vector-set!, vector-increment!, vector-decrement!
(define-synthax vector-stmt
  ([(_ info depth)
    (let* ([vec (expr info (Vector-type (Index-type) (Any-type))
                      #:mutable? #t (- depth 1))]
           [vec-type (infer-type vec)])
      (if (Error-type? vec-type)
          (lifted-error)
          (let* ([index (expr info (Vector-index-type vec-type)
                              #:mutable? #f (- depth 1))]
                 [value (expr info (Vector-output-type vec-type)
                              #:mutable? #f (- depth 1))])
            (choose ((choose vector-increment!^ vector-decrement!^) vec index)
                    (vector-set!^ vec index value)))))]))

;; enum-set-add!, enum-set-remove!
(define-synthax set-stmt
  ([(_ info depth)
    (let* ([sett (expr info (Set-type (Any-type)) #:mutable? #t (- depth 1))]
           [sett-type (infer-type sett)])
      (if (Error-type? sett-type)
          (lifted-error)
          (let* ([elem (expr info (Set-content-type sett-type)
                             #:mutable? #f (- depth 1))])
            (if (lifted-error? elem)
                (lifted-error)
                ((choose enum-set-add!^ enum-set-remove!^) sett elem)))))]))

(define-synthax set!-stmt
  ([(_ info depth)
    (let* ([variable (terminals info (Any-type) #t)])
      ;; TODO: Wrong use of mutability, see comment in grammar.rkt
      (set!^ variable (expr info (infer-type variable) #:mutable? #f depth)))]))

;; If #:mutable is #t, then the return value must be mutable.
;; If #:mutable is #f, then the return value may or may not be mutable.
(define-synthax (expr info desired-type #:mutable? mutable? depth)
  #:base (terminals info desired-type mutable?)
  #:else (choose (terminals info desired-type mutable?)
                 (vector-ref-expr info desired-type #:mutable? mutable? depth)
                 (if mutable? (lifted-error)
                     (base-expr info desired-type depth))
                 (if mutable? (lifted-error)
                     (enum-set-contains-expr info desired-type depth))))

;; vector-ref
(define-synthax vector-ref-expr
  ([(_ info desired-type #:mutable? mutable? depth)
    (let* ([vec (expr info (Vector-type (Index-type) desired-type)
                      #:mutable? mutable? (- depth 1))]
           [vec-type (infer-type vec)])
      (if (Error-type? vec-type)
          (lifted-error)
          (vector-ref^ vec (expr info (Vector-index-type vec-type)
                                 #:mutable? #f (- depth 1)))))]))

;; enum-set-contains?
(define-synthax enum-set-contains-expr
  ([(_ info desired-type depth)
    (if (not (is-supertype? desired-type (Boolean-type)))
        (lifted-error)
        (let* ([sett (expr info (Set-type (Any-type)) #:mutable? #f (- depth 1))]
               [sett-type (infer-type sett)])
          (if (Error-type? sett-type)
              (lifted-error)
              (let* ([elem (expr info (Set-content-type sett-type)
                                 #:mutable? #f (- depth 1))])
                (if (lifted-error? elem)
                    (lifted-error)
                    (enum-set-contains?^ sett elem))))))]))

;; Comparisons, arithmetic, integer holes
(define-synthax base-expr
  ([(_ info desired-type depth)
    (choose
     (if (is-supertype? desired-type (Boolean-type))
         (let* (#;[b1 (expr (Boolean-type) (- depth 1))]
                #;[b2 (expr (Boolean-type) (- depth 1))]
                [i1 (expr info (Integer-type) #:mutable? #f (- depth 1))]
                [i2 (expr info (Integer-type) #:mutable? #f (- depth 1))]
                [e1 (expr info (Any-type) #:mutable? #f (- depth 1))]
                [e2 (expr info (Any-type) #:mutable? #f (- depth 1))])
           (choose ((choose =^ <^) i1 i2)
                   (equal?^ e1 e2)
                   #;(and b1 b2)
                   #;(or b1 b2)
                   #;(not b1)))
         (lifted-error))
     (if (is-supertype? desired-type (Integer-type))
         (let ([i1 (expr info (Integer-type) #:mutable? #f (- depth 1))]
               [i2 (expr info (Integer-type) #:mutable? #f (- depth 1))])
           (choose (?? integer?)
                   ((choose +^ -^ *^) i1 i2)))
         (lifted-error)))]))
