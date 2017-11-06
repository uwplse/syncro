#lang rosette

; Bonsai grammar for Syncro
; https://github.com/uwplse/syncro

; The Bonsai tree data structure
(require "tree-lib.rkt")

; The syntax of our Syncro language, in BNF
; TODO: Improve incrementally to get all the benchmarks through
(define syncro-stx
  '([term one                                     ; integer literal
          (((vector-set! . vec-name) . term) . term)   ; vector set operation
          ((vector-ref . vec-name) . term)             ; vector ref operation
          (op-type . (term . term))               ; see op-type
          name                                    ; variable
    ]
    [type int           ; integer
          vector        ; vector
          any           ; any type
          void          ; void
    ]
    [op-type 
          +             ; addition 
          *             ; multiplication
          -             ; subtraction
    ]
    [name i j] 
    [vec-name permutation inverse-permutation]
    ))

(nonterminals! (harvest syncro-stx))

(define INT (symbol->enum 'int))
(define VEC (symbol->enum 'vector))
(define VOID (symbol->enum 'void))

; Check if t* > t.
(define (is-instance? t t*)
    (tree-match t*
        'int
        (λ () (equal? t INT))

        'vector
        (λ () (equal? t VEC))

        'void
        (λ () (equal? t VOID))

        'any
        (λ () #t)
        
        ))
        
(define (type-expr t env)
    (tree-match t

        'one
        (λ () INT)

        '(((vector-set! . _) . _) . _)
        (λ (vec x y)
            (define vec+ (type-expr vec env))
            (define x+ (type-expr x env))
            (define y+ (type-expr y env))
            (assert (is-instance? vec+ VEC) "vector-set! called on a non-vector!")
            (assert (is-instance? x+ INT) "second arg to vector-set! a non-integer!")
            (assert (is-instance? y+ INT) "third arg to vector-set! a non-integer!")
            VEC)

        '((vector-ref . _) . _)
        (λ (vec x)
            (define vec+ (type-expr vec env))
            (define x+ (type-expr x env))
            (assert (is-instance? vec+ VEC) "vector-ref called on a non-vector!")
            (assert (is-instance? x+ INT) "second arg to vector-ref a non-integer!")
            INT)

        '(_ . (_ . _))
        (λ (op-type x y)
            (define x+ (type-expr x env))
            (define y+ (type-expr y env))
            (assert (is-instance? x+ INT) "Arith op on non-integer!")
            (assert (is-instance? y+ INT) "Arith op on non-integer!")
            INT)

        '_
        (λ (name)
            (define r (table-find* env name))
            (assert r "Undefined variable usage!")
            r)

        ))

(define (eval-expr t env)
    (tree-match t

        'one
        (λ () 1)

        '(((vector-set! . _) . _) . _)
        (λ (vec x y)
            (define vec+ (eval-expr vec env))
            (define x+ (eval-expr x env))
            (define y+ (eval-expr y env))
            (vector-set! vec+ x+ y+)
            vec+)

        '((vector-ref . _) . _)
        (λ (vec x)
            (define vec+ (eval-expr vec env))
            (define x+ (eval-expr x env))
            (vector-ref vec+ x+))

        '(_ . (_._))
        (λ (op-type x y)
            (define x+ (eval-expr x env))
            (define y+ (eval-expr y env))
            (assert (integer? x+) "non-integer")
            (assert (integer? y+) "non-integer")
            (tree-match op-type
                '+
                (λ ()
                    (+ x+ y+))
                '-
                (λ ()
                    (- x+ y+))
                '*
                (λ ()
                    (* x+ y+))
                ))

        '_
        (λ (name)
            (define r (table-find* env name))
            (assert r "Undefined variable usage!")
            r)

        ))

(define test-expr
  (expression->enumtree

    '(((vector-set! . inverse-permutation)
       . ((vector-ref . permutation)
          . i))
      . i)
    ))


(define (depth t)
  (if
    (not (pair? t)) 1
    (+ 1 (max (depth (car t)) (depth (cdr t))))))

(echo test-expr)
(display "Testing expression of size ")
(displayln (depth test-expr))


(displayln "stx  test...")
(assert (syntax-matches? syncro-stx 'term test-expr) "Test stx")

(displayln "type test...")
(define init-type-env '())
(set! init-type-env (table-add init-type-env (symbol->enum 'i) INT))
(set! init-type-env (table-add init-type-env (symbol->enum 'j) INT))
(set! init-type-env (table-add init-type-env (symbol->enum 'permutation) VEC))
(set! init-type-env (table-add init-type-env (symbol->enum 'inverse-permutation) VEC))
; ; (displayln init-type-env)
; (echo (type-expr test-expr init-type-env))

(displayln "eval test...")
(define init-eval-env '())
; (set! init-eval-env (table-add init-eval-env (symbol->enum 'i) 0))
; (set! init-eval-env (table-add init-eval-env (symbol->enum 'j) 1))
; (set! init-eval-env (table-add init-eval-env (symbol->enum 'permutation) (vector 1 2 0)))
; (set! init-eval-env (table-add init-eval-env (symbol->enum 'inverse-permutation) (vector 1 2 0)))
; (echo (eval-expr test-expr init-eval-env))
; (displayln init-eval-env)

(newline)

(displayln "Generating program from IO examples ...")

(displayln "Time for constructing tree for statement 1 ...")
(define stmt1* (time (make! syncro-stx 'term 5)))
(displayln "Time for constructing tree for statement 2 ...")
(define stmt2* (time (make! syncro-stx 'term 5)))
(displayln "Time for typechecking statement 1 ...")
(define typecheck1 (time (type-expr stmt1* init-type-env)))
(displayln "Time for typechecking statement 2 ...")
(define typecheck2 (time (type-expr stmt2* init-type-env)))

(displayln "Time for synthesis by examples")
(define perms          (list (vector 0 1 3 4 2) (vector 1 2 0 3 4) (vector 0 4 1 2 3) (vector 4 1 2 0 3) (vector 4 3 1 2 0)))
(define init-inv-perms (list (vector 0 4 1 2 3) (vector 2 4 1 3 0) (vector 1 2 3 4 0) (vector 1 3 2 4 0) (vector 4 3 2 1 0)))
(define res-inv-perms  (list (vector 0 1 4 2 3) (vector 2 0 1 3 4) (vector 0 2 3 4 1) (vector 3 1 2 4 0) (vector 4 2 3 1 0)))
(define i-list (list 4 0 1 3 2))
(define j-list (list 1 4 0 1 3))

(define synth
   (time (solve (for ([perm perms] [initinv init-inv-perms] [resinv res-inv-perms] [i i-list] [j j-list])
            (set! init-eval-env '())
            (set! init-eval-env (table-add init-eval-env (symbol->enum 'i) i))
            (set! init-eval-env (table-add init-eval-env (symbol->enum 'j) j))
            (set! init-eval-env (table-add init-eval-env (symbol->enum 'permutation) perm))
            (set! init-eval-env (table-add init-eval-env (symbol->enum 'inverse-permutation) initinv))
            (eval-expr stmt1* init-eval-env)
            (assert (equal? (eval-expr stmt2* init-eval-env) resinv))))))

(if (sat? synth)
   (begin 
    (echo (evaluate stmt1* synth))
    (echo (evaluate stmt2* synth)))
   (displayln "No program found"))

(displayln "End of program")
