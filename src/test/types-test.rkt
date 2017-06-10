#lang rosette

(require rackunit rackunit/text-ui)
(require "../rosette/types.rkt" "../rosette/util.rkt")

(provide run-types-tests)

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))

(define-syntax (commutative stx)
  (syntax-case stx ()
    [(_ (check (proc x y) z))
     (syntax/loc stx
       (commutative (check (proc x y) z (format "Failed test: ~a" '(proc x y)))))]
    [(_ (check (proc x y) z message))
     (syntax/loc stx
       (begin (check (proc x y) z message)
              (check (proc y x) z message)))]))

(define tests
  (test-suite
   "Tests for types.rkt"
   (let* ([any (Any-type)]
          [bot (Bottom-type)]
          [idx (Index-type)]
          [bool (Boolean-type)]
          [int (Integer-type)]
          [bv5 (Bitvector-type 5)]
          [vec (Vector-type 10 (Boolean-type))]
          [lst (List-type 3 (Integer-type))]
          [sett (Set-type (Any-type))]
          [dag (DAG-type (Any-type))]
          [rec (Record-type 'foo '(a b) (list int vec))]
          [proc (Procedure-type (list (Vector-type 3 int)) (Integer-type))]
          [rproc (Procedure-type (list (Vector-type 3 int)) int #:read-index 0)]
          [wproc (Procedure-type (list (Vector-type 3 int)) int #:write-index 0)]
          [err (Error-type)]
          [void (Void-type)]
          [all-no-enum-var (list any bot idx bool int bv5 vec lst sett
                                 dag rec proc rproc wproc err void)]
          [enum (Enum-type 'Word 12)]
          [set-enum (Set-type enum)]
          [all-no-var (append all-no-enum-var (list enum set-enum))]
          [vec-var (Vector-type (Type-var idx) bool)]
          [rec-var (Record-type 'foo (list 'a 'b) (list (Type-var) vec-var))]
          [all-no-enum (append all-no-enum-var (list vec-var rec-var))]
          [all (append all-no-var (list vec-var rec-var))])
     (test-case "Constructors, selectors and equality for types"
       (check-exn exn:fail? (thunk (Vector-type bool int)))
       (check-exn exn:fail? (thunk (Vector-type any int)))
       (check-exn exn:fail?
                  (thunk
                   (with-output-to-string
                     (thunk
                      (Record-type 'h (list int) (list int))))))
       (check-exn exn:fail? (thunk (Record-type 'h '(a a) (list int int))))
       
       (check-equal? bot (Bottom-type))
       (check-equal? idx (Index-type))
       (check-equal? bool (Vector-output-type vec))
       (check-equal? int (Vector-index-type vec))
       (check-equal? bv5 (Bitvector-type 5))
       (check-equal? vec (Vector-type 10 bool))
       (check-equal? lst (List-type 3 (Integer-type)))
       (check-equal? sett (Set-type (Any-type)))
       (check-equal? dag (DAG-type (Any-type)))
       (check-equal? proc (Procedure-type (list (Vector-type 3 (Integer-type)))
                                          (Integer-type)))
       (check-not-equal? rproc wproc)
       (check-equal? err (Error-type))
       (check-equal? void (Void-type))

       (check-not-equal? any bot)
       (check-not-equal? idx int)
       (check-not-equal? proc (Procedure-type (list int bool) (Integer-type)))
       (check-not-equal? vec (Vector-type 10 int))
       (check-not-equal? lst (List-type 3 bv5))
       ;; Enums are only equal to themselves
       (check-equal? enum (Enum-type 'Word 12))
       (check-not-equal? enum (Enum-type 'Foo 12))
       (check-not-equal? set-enum (Set-type (Enum-type 'Foo 12)))
       (check-equal? set-enum (Set-type enum))
       ;; New type variables are different from old ones
       (check-not-equal? vec-var (Vector-type (Type-var idx) bool))
       (check-not-equal? rec-var (Record-type 'foo (list 'a 'b)
                                              (list (Type-var) vec-var)))

       (check-equal? (get-record-field-type rec 'a) int)
       (check-equal? (get-record-field-type rec 'b) vec))

     (test-case "get-parent"
       (check-equal? (get-parent bot) any)
       (check-equal? (get-parent idx) any)
       (check-equal? (get-parent bool) any)
       (check-equal? (get-parent int) idx)
       (check-equal? (get-parent bv5) idx)
       (check-equal? (get-parent vec) any)
       (check-equal? (get-parent lst) any)
       (check-equal? (get-parent sett) any)
       (check-equal? (get-parent dag) any)
       (check-equal? (get-parent proc) any)
       (check-equal? (get-parent rproc) any)
       (check-equal? (get-parent wproc) any)
       (check-equal? (get-parent err) any)
       (check-equal? (get-parent void) any)
       (check-equal? (get-parent enum) idx)
       (check-equal? (get-parent set-enum) any)
       (check-equal? (get-parent vec-var) any)
       (check-equal? (get-parent rec-var) any))

     (test-case "Predicates on types"
       (check-false (Type? 12))

       (for ([t all-no-var])
         (check-true (Type? t))
         (check-true (Any-type? t))
         (check-true (is-supertype? any t))
         (check-true (is-supertype? t bot)))

       (define predicate->type
         (hash Bottom-type? bot
               Boolean-type? bool
               Index-type? idx
               Integer-type? int
               Bitvector-type? bv5
               Enum-type? enum
               Vector-type? vec
               List-type? lst
               Set-type? sett
               DAG-type? dag
               Record-type? rec
               Procedure-type? proc
               Error-type? err
               Void-type? void))

       (define expected-true-cases
         (hash Bottom-type? (set bot)
               Boolean-type? (set bot bool)
               Index-type? (set bot idx int bv5 enum)
               Integer-type? (set bot int)
               Bitvector-type? (set bot bv5)
               Enum-type? (set bot enum)
               Vector-type? (set bot vec vec-var)
               List-type? (set bot lst)
               Set-type? (set bot sett set-enum)
               DAG-type? (set bot dag)
               Record-type? (set bot rec rec-var)
               Procedure-type? (set bot proc rproc wproc)
               Error-type? (set bot err)
               Void-type? (set bot void)))

       (for ([pred (hash-keys expected-true-cases)])
         (define expected (hash-ref expected-true-cases pred))
         (define type-for-pred (hash-ref predicate->type pred))
         (for ([t all-no-var])
           (if (set-member? expected t)
               (begin
                 (check-true (pred t)
                             (format "(~a ~a) should be true" pred t))
                 (check-true (is-supertype? type-for-pred t)
                             (format "~a should be a supertype of ~a"
                                     type-for-pred t)))
               (begin
                 (check-false (pred t)
                              (format "(~a ~a) should be false" pred t))
                 (check-false (is-supertype? type-for-pred t)
                              (format "~a should not be a supertype of ~a"
                                      type-for-pred t))))))
       
       (check-true (is-supertype? rproc wproc))
       (check-true (is-supertype? wproc rproc))

       (check-false (is-supertype? bv5 (Bitvector-type 10)))
       (check-false (is-supertype? (Bitvector-type 10) bv5))

       (check-true (symbolic-creator? int))
       (check-false (has-setters? int))
       (check-true (symbolic-creator? vec-var))
       (check-true (has-setters? vec-var))
       (check-false (symbolic-creator? err)))

     (test-case "Recursive supertyping"
       (check-true (is-supertype? (Vector-type int bool)
                                  (Vector-type idx bot)))
       (check-false (is-supertype? (Vector-type idx bool)
                                   (Vector-type int bot)))
       (check-false (is-supertype? (Vector-type int bot)
                                   (Vector-type idx bool)))
       (check-false (is-supertype? (Vector-type idx bot)
                                   (Vector-type int bool)))

       (check-true (is-supertype? set-enum (Set-type enum)))
       (check-false (is-supertype? set-enum (Set-type (Enum-type 'Foo 12))))

       ;; Rec maps a -> int and b -> vec
       (check-true (is-supertype? rec (Record-type 'foo (list 'a 'b 'c)
                                                   (list int vec bool))))
       (check-false (is-supertype? rec (Record-type 'foo (list 'a) (list int))))
       (check-false (is-supertype? rec (Record-type 'foo (list 'a 'b 'c)
                                                    (list bool vec int))))
       (check-true (is-supertype? (Record-type 'foo (list 'b)
                                               (list (Vector-type 10 any)))
                                  rec))
       
       (check-false (is-supertype? (Procedure-type (list int) int)
                                   (Procedure-type (list int int) int)))
       (check-false (is-supertype? (Procedure-type (list int int) int)
                                   (Procedure-type (list int) int)))
       
       (check-true (is-supertype? (Procedure-type (list int) bool)
                                  (Procedure-type (list any) bot)))
       (check-false (is-supertype? (Procedure-type (list any) bool)
                                   (Procedure-type (list int) bot)))
       (check-false (is-supertype? (Procedure-type (list int) bot)
                                   (Procedure-type (list any) bool)))
       (check-false (is-supertype? (Procedure-type (list any) bot)
                                   (Procedure-type (list int) bool)))

       (check-true
        (is-supertype? (Procedure-type (list (Procedure-type (list bool bool)
                                                             (Vector-type idx bool)))
                                       any)
                       (Procedure-type (list (Procedure-type (list bool bool)
                                                             (Vector-type int bool)))
                                       any)))

       (check-false
        (is-supertype? (Procedure-type (list (Procedure-type (list bool bool)
                                                             (Vector-type int bool)))
                                       any)
                       (Procedure-type (list (Procedure-type (list bool bool)
                                                             (Vector-type idx bool)))
                                       any))))

     (test-case "Repr method"
       ;; Special rules for Enums that are not tested here, see types.rkt
       (for ([t all-no-enum])
         (check-equal? t (eval (repr t) ns))))

     (test-case "apply-on-symbolic-type"
       (define-symbolic b1 b2 b3 boolean?)
       (define sym-type
         (if (or b1 b2)
             (Procedure-type (if b2 (list (if b1 int idx)) (list bool bool))
                             (Vector-type 3 (if b1 bool int)))
             (Set-type enum)))
       
       (define (anywhere-symbolic? lst)
         (or (symbolic? lst)
             (and (list? lst) (ormap anywhere-symbolic? lst))))
       (define (concrete-repr x)
         (define result (repr x))
         (when (anywhere-symbolic? result)
           (error (format "Symbolic output ~a" result)))
         result)
       
       (define sym-code
         (apply-on-symbolic-type sym-type concrete-repr))

       (define results
         (hash (and b1 b2)
               '(Procedure-type (list (Integer-type))
                                (Vector-type 3 (Boolean-type))
                                #:read-index #f #:write-index #f)
               
               (and b1 (not b2))
               '(Procedure-type (list (Boolean-type) (Boolean-type))
                                (Vector-type 3 (Boolean-type))
                                #:read-index #f #:write-index #f)
               
               (and (not b1) b2)
               '(Procedure-type (list (Index-type))
                                (Vector-type 3 (Integer-type))
                                #:read-index #f #:write-index #f)
               
               (and (not b1) (not b2))
               '(Set-type (Enum-type 'Word 12))))

       (for ([(condition code) results])
         (define synth (solve (assert condition)))
         (check-equal? (evaluate sym-code synth) code)))

     (test-case "Concrete type map operations"
       (match-define (list a1 a2 a3 a4 a5 a6)
         (build-list 6 (lambda (i) (Type-var))))

       (define (get-final-value tmap key)
         (if (has-binding? tmap key)
             (get-final-value tmap (get-binding tmap key))
             key))

       (check-true (Type-var? (get-final-value (make-type-map) a1)))
       
       (define tmap (make-type-map))
       (check-true (add-type-binding! tmap a1 a2))
       (check-true (add-type-binding! tmap a3 a2))
       (check-true (add-type-binding! tmap a4 a3))
       (check-true (add-type-binding! tmap a5 int))

       (define (check-tmap tmap msg)
         (for ([var (list a1 a2 a3 a4 a6)])
           (check-true (Type-var? (get-final-value tmap var)) msg))
         (check-equal? (get-final-value tmap a5) int msg))

       (check-tmap tmap "Initial tmap")
       ;; Try creating cycles
       (for* ([var1 (list a1 a2 a3 a4)]
              [var2 (remove var1 (list a1 a2 a3 a4))])
         (check-true (add-type-binding! tmap var1 var2))
         (check-tmap tmap (format "Tmap after adding ~a -> ~a" var1 var2)))

       (check-true (add-type-binding! tmap a3 bool))
       (for ([var (list a1 a2 a3 a4)])
         (check-equal? (get-final-value tmap var) bool))
       (check-equal? (get-final-value tmap a5) int)
       (check-true (Type-var? (get-final-value tmap a6)))

       (for ([var (list a1 a2 a3 a4)])
         (check-false (add-type-binding! tmap var a5))
         (check-false (add-type-binding! tmap a5 var))))

     (test-case "Unification without type variables"
       ;; Don't compare multiple read and write procedures since unification
       ;; is not smart about read and write indexes
       ;; Ignore type variables because supertyping doesn't play well.
       (define almost-all (remove rproc (remove wproc all-no-var)))
       (for* ([t1 almost-all]
              [t2 almost-all])
         (cond [(is-supertype? t1 t2)
                (check-equal?
                 (unify-types t1 t2) t2
                 (format "~a is a supertype of ~a but unification does not give ~a"
                         t1 t2 t2))]
               [(is-supertype? t2 t1)
                (check-equal?
                 (unify-types t1 t2) t1
                 (format "~a is a supertype of ~a but unification does not give ~a"
                         t2 t1 t1))]
               [else
                (check-false (unify-types t1 t2)
                             (format "~a and ~a have no subtyping relation but unification did not fail"
                                     t1 t2))])))

     (test-case "Unification with type variables"
       (define a1 (Type-var (Index-type)))
       (match-define (list a2 a3)
         (build-list 2 (lambda (i) (Type-var))))

       (commutative (check-equal? (unify-types int a1) int))
       (commutative
        (check-equal? (unify-types (Vector-type a1 int) (Vector-type int a2))
                      (Vector-type int int)))
       (commutative
        (check-equal? (unify-types set-enum (Set-type a1)) set-enum))

       (check-false (unify-types (Vector-type a1 int)
                                 (Vector-type int bool)))

       (define new-rec-type
         (unify-types rec-var
                      (Record-type 'foo '(c a b) (list idx sett vec-var))))
       (check-equal? (get-record-field-type new-rec-type 'a) sett)
       (check-equal? (get-record-field-type new-rec-type 'b) vec-var)

       (commutative
        (check-equal? (unify-types
                       (Procedure-type (list (Vector-type a1 a2) a1) a2)
                       (Procedure-type (list (Vector-type int a1) a3) int))
                      (Procedure-type (list (Vector-type int int) int) int)))
       ;; Renaming of type variables is important here
       (check-equal? (unify-types
                      (Procedure-type (list (Vector-type a1 a2) a1) a2)
                      (Procedure-type (list (Vector-type int a1) a2) enum))
                     (Procedure-type (list (Vector-type int enum) int) enum))
       ;; Here even taking quantification into account we cannot unify.
       ;; The first type must have a1 = int and a2 = bool
       ;; But then the second type must have a1 = int and a1 = bool.
       (check-false (unify-types
                     (Procedure-type (list (Vector-type a1 a2) a1) a2)
                     (Procedure-type (list (Vector-type int a1) a1) enum))))

     (test-case "apply-type"
       (match-define (list a1 a2)
         (build-list 2 (lambda (i) (Type-var))))
       (define a1-idx (Type-var idx))

       ;; Simple examples solvable with direct equality
       (check-equal? (apply-type (Procedure-type '() int) '()) int)
       (check-equal? (apply-type (Procedure-type (list int) bool)
                                 (list int))
                     bool)
       (check-false (apply-type (Procedure-type (list int) bool) (list int int)))
       (check-false (apply-type (Procedure-type (list int) bool) (list bool)))

       ;; Examples that require subtyping reasoning
       (check-equal? (apply-type (Procedure-type (list idx) bool)
                                 (list int))
                     bool)
       (check-equal? (apply-type (Procedure-type (list int) bool)
                                 (list idx))
                     bool)

       ;; Examples that require unification and subtyping
       (check-equal? (apply-type
                      (Procedure-type (list (Vector-type idx a2) a1) a2)
                      (list vec (Vector-index-type vec)))
                     (Vector-output-type vec))
       
       (check-false (apply-type
                     (Procedure-type (list (Vector-type a1-idx a2) a1-idx) a2)
                     (list vec bool))))

     (test-case "get-domain-given-range"
       (check-equal? (get-domain-given-range proc int)
                     (list (Vector-type 3 int)))

       (match-define (list alpha-v1 alpha-v2)
         (build-list 2 (lambda (i) (Type-var (Index-type)))))
       (match-define (list beta-v1 beta-v2)
         (build-list 2 (lambda (i) (Type-var))))
       
       ;; Examples: Vector operations
       ;; vector-set!
       (define vector-set!-type
         (Procedure-type (list (Vector-type alpha-v1 beta-v1) alpha-v1 beta-v1)
                         void))
       ;; Type variables can get renamed so can't just use check-equal?
       (match (get-domain-given-range vector-set!-type void)
         [`(,vec-type ,alpha-type ,beta-type)
          (check-true (and (Type-var? alpha-type) (Type-var? beta-type)))
          (check-equal? (Vector-index-type vec-type) alpha-type)
          (check-equal? (Vector-output-type vec-type) beta-type)])
       ;; If we use defaults then we don't have to worry about renaming
       (check-equal? (get-domain-given-range vector-set!-type void #t)
                     (list (Vector-type idx any) idx any))
       
       ;; vector-ref
       (define vector-ref-type
         (Procedure-type (list (Vector-type alpha-v2 beta-v2) alpha-v2)
                         beta-v2))
       (match (get-domain-given-range vector-ref-type int)
         [`(,vec-type ,alpha-type)
          (check-true (Type-var? alpha-type))
          (check-equal? (Vector-index-type vec-type) alpha-type)
          (check-equal? (Vector-output-type vec-type) int)])
       (check-equal? (get-domain-given-range vector-ref-type int #t)
                     (list (Vector-type idx int) idx))))))

;; TODO: Tests for the symbolic code generation

(define (run-types-tests)
  (displayln "Running tests for types.rkt")
  (run-tests tests))

(module+ main
  (run-types-tests))
