#lang rosette

; Bonsai grammar for Syncro
; https://github.com/uwplse/syncro

; The Bonsai tree data structure
(require "tree-lib.rkt")

; The syntax of our Syncro language, in BNF
(define syncro-stx
  '([stmt (((vector-set! . vecb-term) . int-term) . bool-term)
          (((vector-set! . vecl-term) . int-term) . list-term)
          void-stmt
          (((if . bool-term) . stmt) . stmt)
    ]
    [vecl-term r1]
    [vecb-term ψ]
    [listb-term 
        ((vector-ref . vecl-term) . int-term)
        null
        ((cons . bool-term) . listb-term)
    ]
    [int-term
        one
        i
    ]
    [bool-term 
        true
        false
        formula
        ((list-ref . listb-term) . int-term)
        ((vector-ref . vecb-term) . int-term)
    ]
    [type int           ; integer
          vecb          ; vector of bools
          vecl          ; vector of lists
          listb         ; list of bools
          bool          ; boolean
          void          ; void
          any           ; any type
    ]
    ))

(nonterminals! (harvest syncro-stx))

(define INT (symbol->enum 'int))
(define VECB (symbol->enum 'vecb))
(define VECL (symbol->enum 'vecl))
(define LISTB (symbol->enum 'listb))
(define BOOL (symbol->enum 'bool))
(define VOID (symbol->enum 'void))

; Check if t* > t.
(define (is-instance? t t*)
    (tree-match t*
        'int
        (λ () (equal? t INT))

        'vecb
        (λ () (equal? t VECB))

        'vecl
        (λ () (equal? t VECL))

        'listb
        (λ () (equal? t LISTB))
        
        'bool
        (λ () (equal? t BOOL))

        'void
        (λ () (equal? t VOID))

        'any
        (λ () #t)
        
        ))
        
(define (type-expr t env)
    (tree-match t

        'void-stmt
        (λ () INT)

        'one
        (λ () INT)

        'true
        (λ () BOOL)

        'false
        (λ () BOOL)

        'null
        (λ () LISTB)

        '((cons . _) . _)
        (λ (b list)
            (define list+ (type-expr list env))
            (define b+ (type-expr b env))
            (assert (is-instance? b+ BOOL) "cons on a non-bool!")
            (assert (is-instance? list+ LISTB) "second arg to cons not list type!")
            LISTB)
       
        '((list-ref . _) . _)
        (λ (list x)
            (define list+ (type-expr list env))
            (define x+ (type-expr x env))
            (assert (is-instance? list+ LISTB) "list-ref first arg non-list!")
            (assert (is-instance? x+ INT) "list-ref second arg non-integer!")
            BOOL)

        '((vector-ref . _) . _)
        (λ (vec x)
            (define vec+ (type-expr vec env))
            (define x+ (type-expr x env))
            (assert (or (is-instance? vec+ VECL) (is-instance? vec+ VECB)) "vector-ref first arg non-vector!")
            (assert (is-instance? x+ INT) "vector-ref second arg non-integer!")
            (if (is-instance? vec+ VECL) LISTB BOOL))

        '(((vector-set! . _) . _) . _)
        (λ (vec x y)
            (define vec+ (type-expr vec env))
            (define x+ (type-expr x env))
            (define y+ (type-expr y env))
            (assert (or (is-instance? vec+ VECL) (is-instance? vec+ VECB)) "vector-set! first arg non-vector!")
            (assert (is-instance? x+ INT) "second arg to vector-set! a non-integer!")
            (assert (if (is-instance? vec+ VECL) (is-instance? y+ LISTB) (is-instance? y+ BOOL)) "third arg to vector-set! is wrong type!")
            INT)
       
        '((if . _) _ . _)
        (λ (c t f)
            (define c+ (type-expr c env))
            (define t+ (type-expr t env))
            (define f+ (type-expr f env))
            (assert (is-instance? c+ BOOL))
            (assert (equal? t+ f+))
            t+)

        '_
        (λ (name)
            (define r (table-find* env name))
            (assert r "Undefined variable usage!")
            r)

        ))

(define (eval-expr t env)
    (tree-match t

        'void-stmt
        (λ () 0)

        'one
        (λ () 1)

        'true
        (λ () true)

        'false
        (λ () false)

        'null
        (λ () null)

        '((cons . _) . _)
        (λ (b list)
            (define list+ (eval-expr list env))
            (define b+ (eval-expr b env))
            (cons b list))
       
        '((list-ref . _) . _)
        (λ (list x)
            (define list+ (type-expr list env))
            (define x+ (type-expr x env))
            (assert (is-instance? list+ LISTB) "list-ref first arg non-list!")
            (assert (is-instance? x+ INT) "list-ref second arg non-integer!")
            BOOL)

        '((vector-ref . _) . _)
        (λ (vec x)
            (define vec+ (eval-expr vec env))
            (define x+ (eval-expr x env))
            (vector-ref vec+ x+))

        '(((vector-set! . _) . _) . _)
        (λ (vec x y)
            (define vec+ (eval-expr vec env))
            (define x+ (eval-expr x env))
            (define y+ (eval-expr y env))
            (vector-set! vec+ x+ y+)
            0)

        '((if . _) _ . _)
        (λ (c t f)
            (define c+ (eval-expr c env))
            (define t+ (eval-expr t env))
            (define f+ (eval-expr f env))
            (assert (boolean? c+))
            (if c+ t+ f+))

        '_
        (λ (name)
            (define r (table-find* env name))
            (assert r "Undefined variable usage!")
            r)

        ))

(define test-expr
  (expression->enumtree
    '(((if . formula) .  
        (((vector-set! . ψ) . i) . false)) . void-stmt)
    ))

(define (depth t)
  (if
    (not (pair? t)) 1
    (+ 1 (max (depth (car t)) (depth (cdr t))))))

(echo test-expr)
(display "Testing expression of size ")
(displayln (depth test-expr))


(displayln "stx  test...")
(assert (syntax-matches? syncro-stx 'stmt test-expr) "Test stx")

(displayln "type test...")
(define init-type-env '())
(set! init-type-env (table-add init-type-env (symbol->enum 'r1) VECL))
(set! init-type-env (table-add init-type-env (symbol->enum 'ψ) VECB))
(set! init-type-env (table-add init-type-env (symbol->enum 'i) INT))
(set! init-type-env (table-add init-type-env (symbol->enum 'formula) BOOL))
; ; (displayln init-type-env)
; (echo (type-expr test-expr init-type-env))

(displayln "eval test...")
(define init-eval-env '())
; ; (set! init-eval-env (table-add init-eval-env (symbol->enum 'i) 0))
; ; (set! init-eval-env (table-add init-eval-env (symbol->enum 'j) 1))
; ; (set! init-eval-env (table-add init-eval-env (symbol->enum 'permutation) (vector 1 2 0)))
; ; (set! init-eval-env (table-add init-eval-env (symbol->enum 'inverse-permutation) (vector 1 2 0)))
; ; (echo (eval-expr test-expr init-eval-env))
; ; (displayln init-eval-env)

(newline)

; (displayln "Generating program from IO examples ...")

(displayln "Time for constructing tree for statement 1 ...")
(define stmt1* (time (make! syncro-stx 'stmt 6)))
(displayln "Time for typechecking statement 1 ...")
(define typecheck1 (time (type-expr stmt1* init-type-env)))

(displayln "Time for synthesis by examples")

(define input-output-examples
  (list (list (vector '(false false) '(true)) 1 true '(true false) '(true false))
        (list (vector '(false true true) '()) 0 false '(false true) '(false true))
        (list (vector '(true true false) '(true)) 1 true '(false false) '(false false))))

(define synth
   (time (solve (for ([parameters input-output-examples])
            (match-define (list r1 i formula old-ψ new-ψ)
                parameters)
            (set! init-eval-env '())
            (set! init-eval-env (table-add init-eval-env (symbol->enum 'ψ) old-ψ))
            (set! init-eval-env (table-add init-eval-env (symbol->enum 'r1) r1))
            (set! init-eval-env (table-add init-eval-env (symbol->enum 'i) i))
            (set! init-eval-env (table-add init-eval-env (symbol->enum 'formula) formula))
            (assert (equal? (eval-expr stmt1* init-eval-env) new-ψ))))))

(if (sat? synth)
   (begin 
    (echo (evaluate stmt1* synth)))
   (displayln "No program found"))

(displayln "End of program")
