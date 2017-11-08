#lang rosette

; Bonsai grammar for Syncro
; https://github.com/uwplse/syncro

; The Bonsai tree data structure
(require "tree-lib.rkt")

; The syntax of our Syncro language, in BNF
(define syncro-stx
  '([stmt ((vector-increment! . vec-term) . int-term)             ; vector incr operation
          ((vector-decrement! . vec-term) . int-term)             ; vector decr operation
          void-stmt
          (((if . bool-term) . stmt) . stmt)
    ]
    [vec-term
        vec-name
        ((vector-ref . vecvec-name) . int-term)
    ]
    [int-term
        one
        zero
        name
        ((vector-ref . vec-term) . int-term)
    ]
    [bool-term 
          true
          false
          ((= . int-term) . int-term)                 ; equality check
    ]
    [vecvec-name num2helper]
    [vec-name num2 word2doc]
    [name word old-topic new-topic] 
    [type int           ; integer
          vector        ; vector
          vecvec        ; vector of vector
          bool          ; boolean
          ; eDocument     ; enum Document
          ; eWord         ; enum Word
          ; eTopic        ; enum Topic
          any           ; any type
          void          ; void
    ]
    ))

(nonterminals! (harvest syncro-stx))

(define INT (symbol->enum 'int))
(define VEC (symbol->enum 'vector))
(define BOOL (symbol->enum 'bool))
(define VECVEC (symbol->enum 'vecvec))
(define VOID (symbol->enum 'void))

; Check if t* > t.
(define (is-instance? t t*)
    (tree-match t*
        'int
        (λ () (equal? t INT))

        'vector
        (λ () (equal? t VEC))

        'bool
        (λ () (equal? t BOOL))

        'vecvec
        (λ () (equal? t VECVEC))

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

        'zero
        (λ () INT)

        'true
        (λ () BOOL)

        'false
        (λ () BOOL)

        '((=  . _) . _)
        (λ (x y)
            (define x+ (type-expr x env))
            (define y+ (type-expr y env))
            (assert (is-instance? x+ INT) "Equality on a non-integer!")
            (assert (is-instance? y+ INT) "Equality on a non-integer!")
            BOOL)

        '((vector-ref . _) . _)
        (λ (vec x)
            (define vec+ (type-expr vec env))
            (define x+ (type-expr x env))
            (assert (or (is-instance? vec+ VEC) (is-instance? vec+ VECVEC)) "vector-increment! first arg non-vector!")
            (assert (is-instance? x+ INT) "vector-increment! second arg non-integer!")
            (if (is-instance? vec+ VEC) INT VEC))

        '((vector-increment! . _) . _)
        (λ (vec x)
            (define vec+ (type-expr vec env))
            (define x+ (type-expr x env))
            (assert (is-instance? vec+ VEC) "vector-increment! first arg non-vector!")
            (assert (is-instance? x+ INT) "vector-increment! second arg non-integer!")
            INT)

        '((vector-decrement! . _) . _)
        (λ (vec x)
            (define vec+ (type-expr vec env))
            (define x+ (type-expr x env))
            (assert (is-instance? vec+ VEC) "vector-increment! first arg non-vector!")
            (assert (is-instance? x+ INT) "vector-increment! second arg non-integer!")
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

        'zero
        (λ () 0)

        'true
        (λ () true)

        'false
        (λ () false)

        '((=  . _) . _)
        (λ (x y)
            (define x+ (eval-expr x env))
            (define y+ (eval-expr y env))
            (= x+ y+))

        '((vector-ref . _) . _)
        (λ (vec x)
            (define vec+ (eval-expr vec env))
            (define x+ (eval-expr x env))
            (vector-ref vec+ x+))

        '((vector-increment! . _) . _)
        (λ (vec x)
            (define vec+ (eval-expr vec env))
            (define x+ (eval-expr x env))
            (vector-set! vec+ (+ (vector-ref vec+ x+) 1))
            0)

        '((vector-decrement! . _) . _)
        (λ (vec x)
            (define vec+ (eval-expr vec env))
            (define x+ (eval-expr x env))
            (vector-set! vec+ (- (vector-ref vec+ x+) 1))
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
    '(((if . ((= . ((vector-ref . ((vector-ref . num2helper) . ((vector-ref . word2doc) . word))) . old-topic)) . one)) .  
        ((vector-decrement! . num2) . ((vector-ref . word2doc) . word))) . void-stmt)
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
(set! init-type-env (table-add init-type-env (symbol->enum 'num2) VEC))
(set! init-type-env (table-add init-type-env (symbol->enum 'num2helper) VECVEC))
(set! init-type-env (table-add init-type-env (symbol->enum 'old-topic) INT))
(set! init-type-env (table-add init-type-env (symbol->enum 'new-topic) INT))
(set! init-type-env (table-add init-type-env (symbol->enum 'word) INT))
(set! init-type-env (table-add init-type-env (symbol->enum 'word2doc) VEC))
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
(define stmt1* (time (make! syncro-stx 'stmt 11)))
(displayln "Time for constructing tree for statement 2 ...")
(define stmt2* (time (make! syncro-stx 'stmt 11)))
(displayln "Time for typechecking statement 1 ...")
(define typecheck1 (time (type-expr stmt1* init-type-env)))
(displayln "Time for typechecking statement 2 ...")
(define typecheck2 (time (type-expr stmt2* init-type-env)))

(displayln "Time for synthesis by examples")
(define (matrixify list-of-lists)
  (list->vector (map list->vector list-of-lists)))

(define word->document (vector 0 0 0 0 0 1 1 1 1 1 1 1))

(define input-output-examples
  (list (list (matrixify '((2 2 1) (5 2 0))) 3 0 1 (vector 3 2) (vector 3 2))
        (list (matrixify '((2 2 1) (5 2 0))) 1 2 1 (vector 3 2) (vector 2 2))
        (list (matrixify '((2 2 1) (5 2 0))) 9 1 0 (vector 3 2) (vector 3 2))
        (list (matrixify '((2 2 1) (5 2 0))) 7 0 2 (vector 3 2) (vector 3 3))
        (list (matrixify '((0 1 4) (5 2 0))) 0 1 0 (vector 2 2) (vector 2 2))
        (list (matrixify '((0 1 4) (5 2 0))) 4 1 2 (vector 2 2) (vector 1 2))
        (list (matrixify '((0 1 4) (5 2 0))) 8 0 2 (vector 2 2) (vector 2 3))))

(define synth
   (time (solve (for ([parameters input-output-examples])
            (match-define (list num2helper word old-topic new-topic old-num2 new-num2)
                parameters)
            (set! init-eval-env '())
            (set! init-eval-env (table-add init-eval-env (symbol->enum 'num2) old-num2))
            (set! init-eval-env (table-add init-eval-env (symbol->enum 'num2helper) num2helper))
            (set! init-eval-env (table-add init-eval-env (symbol->enum 'old-topic) old-topic))
            (set! init-eval-env (table-add init-eval-env (symbol->enum 'new-topic) new-topic))
            (set! init-eval-env (table-add init-eval-env (symbol->enum 'word) word))
            (set! init-eval-env (table-add init-eval-env (symbol->enum 'word2doc) word->document))
            (eval-expr stmt1* init-eval-env)
            (assert (equal? (eval-expr stmt2* init-eval-env) new-num2))))))


(if (sat? synth)
   (begin 
    (echo (evaluate stmt1* synth))
    (echo (evaluate stmt2* synth)))
   (displayln "No program found"))

(displayln "End of program")
