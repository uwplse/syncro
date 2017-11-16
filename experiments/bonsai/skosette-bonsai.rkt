#lang rosette

; Bonsai grammar for Syncro
; https://github.com/uwplse/syncro

; The Bonsai tree data structure
(require "tree-lib.rkt")

; The syntax of our Syncro language, in BNF
(define syncro-stx
  '([stmt 
          (((vector-set! . vecb-term) . int-term) . bool-term)
          (((vector-set! . vecl-term) . int-term) . list-term)
          void-stmt
          (((if . bool-term) . stmt) . stmt)
    ]
    [listb-term 
        ((vector-ref . vecl-term) . int-term)
        null
        ((cons . bool-term) . listb-term)
    ]
    [int-term
        one
        int-names
    ]
    [bool-term 
        true
        false
        bool-names
        ((list-ref . listb-term) . int-term)
        ((vector-ref . vecb-term) . int-term)
    ]
    [vecl-term r1]
    [vecb-term ψ]
    [int-names i]
    [bool-names formula]
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
        (λ (b lst)
            (define lst+ (type-expr lst env))
            (define b+ (type-expr b env))
            (assert (is-instance? b+ BOOL) "cons on a non-bool!")
            (assert (is-instance? lst+ LISTB) "second arg to cons not list type!")
            LISTB)
       
        '((list-ref . _) . _)
        (λ (lst x)
            (define lst+ (type-expr lst env))
            (define x+ (type-expr x env))
            (assert (is-instance? lst+ LISTB) "list-ref first arg non-list!")
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
       
        '(((if . _) . _ ) . _)
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
        (λ () #t)

        'false
        (λ () #f)

        'null
        (λ () '())

        '((cons . _) . _)
        (λ (b list)
            (define list+ (eval-expr list env))
            (define b+ (eval-expr b env))
            (cons b+ list+))

        '((list-ref . _) . _)
        (λ (list x)
            (define list+ (type-expr list env))
            (define x+ (type-expr x env))
            (list-ref list+ x+))

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

        '(((if . _) . _ ) . _)
        (λ (c t f)
            (define c+ (eval-expr c env))
            (assert (boolean? c+))
            (if c+ (eval-expr t env) (eval-expr f env)))

        '_
        (λ (name)
            (define r (table-find* env name))
            r)

        ))

(define test-expr
  (expression->enumtree
    '(((if . formula) . (((vector-set! . ψ) . i) . false)) . void-stmt)
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
; (displayln init-type-env)
(echo (type-expr test-expr init-type-env))
    
(displayln "eval test...")
(define init-eval-env '())
; ; (set! init-eval-env (table-add init-eval-env (symbol->enum 'i) 0))
; ; (set! init-eval-env (table-add init-eval-env (symbol->enum 'j) 1))
; ; (set! init-eval-env (table-add init-eval-env (symbol->enum 'permutation) (vector 1 2 0)))
; ; (set! init-eval-env (table-add init-eval-env (symbol->enum 'inverse-permutation) (vector 1 2 0)))
; ; (echo (eval-expr test-expr init-eval-env))
; ; (displayln init-eval-env)

(newline)

(displayln "Generating program from IO examples ...")

(displayln "Time for constructing tree for statement 1 ...")
(define stmt1* (time (make! syncro-stx 'stmt 6)))
; (define stmt1* (time test-expr))
(displayln "Time for typechecking statement 1 ...")
(define typecheck1 (time (type-expr stmt1* init-type-env)))

(displayln "Time for synthesis by examples")
(define input-output-examples
  (list
   (list (vector '(#f #f)) (vector '(#t #f #f)) 0 #t
         (vector #t) (vector #f))
   (list (vector '(#f #f)) (vector '(#f #f #f)) 0 #f
         (vector #t) (vector #t))
   (list (vector '(#t)) (vector '(#t #t)) 0 #t
         (vector #f) (vector #f))
   (list (vector '(#t #f #f)) (vector '(#f #t #f #f)) 0 #f
         (vector #f) (vector #f))
   (list (vector '(#f #f) '(#t)) (vector '(#t #f #f) '(#t)) 0 #t
         (vector #t #f) (vector #f #f))
   (list (vector '(#f #t #t) '(#f)) (vector '(#f #t #t) '(#f #f)) 1 #f
         (vector #f #t) (vector #f #t))
   (list (vector '(#f #t #t) '(#f)) (vector '(#f #t #t) '(#t #f)) 1 #t
         (vector #f #t) (vector #f #f))
   (list (vector '(#t #t #f) '(#t)) (vector '(#t #t #f) '(#t #t)) 1 #t
         (vector #f #f) (vector #f #f))
   (list (vector '(#f #f #f) '(#f)) (vector '(#f #f #f #f) '(#f)) 0 #f
         (vector #t #t) (vector #t #t))
   (list (vector '(#f #f #f) '(#f)) (vector '(#f #f #f) '(#f #f)) 1 #f
         (vector #t #t) (vector #t #t))
   (list (vector '(#f) '(#f) '(#f #f)) (vector '(#f) '(#f) '(#f #f #f)) 2 #f
         (vector #t #t #t) (vector #t #t #t))
   (list (vector '(#f) '(#f) '(#f #f)) (vector '(#f) '(#f) '(#t #f #f)) 2 #t
         (vector #t #t #t) (vector #t #t #f))))

(define synth
   (time (solve (for ([parameters input-output-examples])
            (match-define (list r1 r1-after-update i form old-ψ new-ψ)
                parameters)
            (set! init-eval-env '())
            (set! init-eval-env (table-add init-eval-env (symbol->enum 'ψ) old-ψ))
            (set! init-eval-env (table-add init-eval-env (symbol->enum 'r1) r1))
            (set! init-eval-env (table-add init-eval-env (symbol->enum 'i) i))
            (set! init-eval-env (table-add init-eval-env (symbol->enum 'formula) form))
            (eval-expr stmt1* init-eval-env)
            (assert (equal? (table-find* init-eval-env (symbol->enum 'ψ)) new-ψ) "IO assert failed")))))

(if (sat? synth)
   (begin 
    (echo (evaluate stmt1* synth)))
   (displayln "No program found"))

(displayln "End of program")
