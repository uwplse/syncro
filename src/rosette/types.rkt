#lang rosette

(require (for-syntax syntax/parse (only-in racket/syntax format-id))
         rosette/lib/angelic
         "enum-set.rkt" "graph.rkt" "map.rkt" "record.rkt" "symhash.rkt"
         "util.rkt")

(provide
 ;; Constructors
 Any-type Bottom-type define-type-alias define-base-type
 Boolean-type Index-type Integer-type Bitvector-type Enum-type
 Vector-type Set-type Map-type DAG-type Record-type define-record
 Procedure-type Error-type Void-type Type-var
 ;; The repr method for type variables must use Type-Var
 Type-Var

 ;; Predicates on types
 Any-type? Bottom-type? Alias-type? Base-type?
 Boolean-type? Index-type? Integer-type? Bitvector-type? Enum-type?
 Vector-type? Set-type? DAG-type? Record-type? Procedure-type?
 Error-type? Void-type? (rename-out [Type-Var? Type-var?])

 ;; Selectors (for some types)
 (rename-out [Vector-Type-index-type Vector-index-type]
             [Vector-Type-output-type Vector-output-type]
             [Set-Type-content-type Set-content-type]
             [DAG-Type-vertex-type DAG-vertex-type]
             [Record-Type-constructor Record-constructor]
             [Record-Type-fields Record-fields]
             [Record-Type-field-types Record-field-types]
             [Record-Type-field-constant? Record-field-constant?]
             [Procedure-Type-domain-types Procedure-domain-types]
             [Procedure-Type-range-type Procedure-range-type]
             [Procedure-Type-read-index Procedure-read-index]
             [Procedure-Type-write-index Procedure-write-index])
 get-record-field-type can-mutate-record-field?

 ;; Type mutability pairs
 (struct-out tm-pair)

 ;; Generic function stuff
 gen:Type Type? gen:symbolic symbolic?

 ;; General utility functions
 get-parent is-supertype? repr apply-on-symbolic-type

 ;; Useful functions for type analysis
 ;; Higher level functions
 unify-types apply-type get-domain-given-range
 ;; Lower level functions (for constraint generation)
 make-type-map unify make-fresh replace-type-vars get-free-type-vars
 has-binding? get-binding add-type-binding!

 ;; Useful functions for mutability analysis
 has-setters? apply-type-with-mutability
 get-domain-given-range-with-mutability is-application-mutable?

 ;; Operations used for symbolic code generation
 make-symbolic symbolic-code generate-update-arg-names
 update-code old-values-code symbolic-update-code)

;; Creates type predicates that properly handle Bottom types.
(define-syntax (make-type-predicates stx)
  (syntax-case stx ()
    [(_ (pred sym) ...)
     (syntax/loc stx
       (begin (define (sym x)
                (or (Bottom-Type? x) (pred x)))
              ...))]))

(make-type-predicates
 [Any-Type? Any-type?] [Bottom-Type? Bottom-type?]
 [Alias-Type? Alias-type?] [Base-Type? Base-type?]
 [Boolean-Type? Boolean-type?] [Index-Type? Index-type?]
 [Integer-Type? Integer-type?] [Bitvector-Type? Bitvector-type?]
 [Enum-Type? Enum-type?]
 [Vector-Type? Vector-type?] [Set-Type? Set-type?] [Map-Type? Map-type?]
 [DAG-Type? DAG-type?] [Record-Type? Record-type?]
 [Procedure-Type? Procedure-type?]
 [Error-Type? Error-type?] [Void-Type? Void-type?])


(define-generics Type
  ;; Returns an instance of the parent type with the same fields.
  (get-parent Type)
  (typeof-predicate Type)
  ;; Returns #t if type is a supertype of other-type, #f otherwise
  ;; Can only be called on types that do not contain type variables.
  ;; TODO: Deprecate. Behavior can be achieved using unify.
  (is-supertype? Type other-type)
  ;; Returns an S-expression that could be used to create a new
  ;; instance of the type.
  (repr Type)
  ;; The input must be a type that is not a symbolic union (although
  ;; it may contain symbolic unions.
  ;; For example, (Vector-type Int {Int | Bool}) is a valid input
  ;; Concretizes any types inside the type and applies fn to each such
  ;; concrete type, and then merges these to return a symbolic value.
  ;; (This is done by using for/all.)
  ;; Note: Due to a bug in Rosette, currently the Type argument must
  ;; come first.
  (apply-on-symbolic-type-helper Type fn)
  ;; Unifies two types for type inference.
  ;; mapping is a type map that stores the unification bindings so far
  ;; (see bottom of this file).
  ;; Returns the unified type, or #f if unification is impossible.
  (unify-helper Type other-type mapping)
  ;; Returns a list of the free type variables in the type.
  (get-free-type-vars Type)
  ;; Replaces type variables in this type with their values as given
  ;; by the type mapping.
  (replace-type-vars Type mapping [default])

  #:fallbacks
  [(define (apply-on-symbolic-type-helper self fn)
     ;; Works for any type that doesn't contain other types within it.
     (fn self))

   ;; Most types can never have type variables inside themselves
   (define (get-free-type-vars self) '())
   (define (replace-type-vars self mapping [default #f]) self)])

;; Like apply-on-symbolic-type-helper, but can take symbolic unions as inputs
;; as well.
(define (apply-on-symbolic-type type fn)
  (for/all ([type type])
    (apply-on-symbolic-type-helper type fn)))

;; TODO: Does this work if the list has symbolic length?
;; I think so, because lists with different lengths have
;; different guards, and so after a for/all the list should have
;; concrete length. Worth checking though.
(define (apply-on-symbolic-type-list lst fn)
  (if (null? lst)
      (fn lst)
      (apply-on-symbolic-type
       (car lst)
       (lambda (c-type)
         (apply-on-symbolic-type-list
          (cdr lst)
          (lambda (c-lst)
            (fn (cons c-type c-lst))))))))

(define (unify-types t1 t2)
  (let ([mapping (make-type-map)])
    (define result (unify t1 t2 mapping))
    (and result (replace-type-vars result mapping))))

;; Unifies types assuming there are no type variables inside self.
;; other-type may be a type variable.
(define (unify t1 t2 mapping)
  (for*/all ([t1 t1] [t2 t2])
    (cond [(Type-Var? t1)
           (unify-helper t1 t2 mapping)]
          [(Type-Var? t2)
           (unify-helper t2 t1 mapping)]
          [(Bottom-Type? t1) t1]
          [(Bottom-Type? t2) t2]
          [((typeof-predicate t1) t2)
           (unify-helper t1 t2 mapping)]
          [((typeof-predicate t2) t1)
           (unify-helper t2 t1 mapping)]
          [else #f])))

(define-generics symbolic
  ;; Returns #t if it is possible to modify elements in a value of
  ;; this type (eg. Vectors), #f otherwise (eg. Booleans)
  (has-setters? symbolic)
  ;; TODO
  (make-symbolic symbolic varset)
  ;; For a given kind of update to this type (eg. assignment),
  ;; generates argument names that would be used in the update
  ;; function.
  ;; For example, for update type 'assign to a vector v, we might
  ;; add the mapping 'assign -> '(index1 val2)
  ;; Then the update procedure should look like
  ;; (define (assign-v! index1 val2) ...)
  (generate-update-arg-names symbolic update-type)
  ;; Returns a function that, given the update argument names,
  ;; produces the code that performs the update.
  ;; For example, for update type 'assign to a vector v with update
  ;; names '(v index1 val2), we would generate
  ;; (vector-set! v index1 val2)
  ;; TODO: The interface here is different from everything else. Fix.
  (update-code symbolic update-type)
  ;; Returns various things that are important for defining which
  ;; values are overwritten by a particular update.
  ;; TODO: Document better
  (old-values-code symbolic update-type var . update-args)
  ;; Like update code, but does things symbolically. Interface is also
  ;; different.
  ;; TODO: Document better
  (symbolic-update-code symbolic update-type var update-args varset-name))

(define (make-rosette-val rosette-type varset)
  (define-symbolic* val rosette-type)
  (when varset (set-add! varset (make-input val '())))
  val)

(define (make-bounded-val low high rosette-type varset)
  (define-symbolic* bounded-val rosette-type)
  (when varset
    (define assertions (list (>= bounded-val low) (< bounded-val high)))
    (set-add! varset (make-input bounded-val assertions)))
  bounded-val)

;; Returns syntax that creates a symbolic value of this type
;; assigned to the variable var. The generated code also adds all
;; symbolic variables to varset-name, which is a symbol that at
;; runtime will have a set as a value. If varset-name is #f, that
;; code is not generated.
(define (symbolic-code type var [varset-name #f])
  #`(define #,var (make-symbolic #,(repr type) #,varset-name)))

(struct Any-Type () #:transparent
  #:methods gen:Type
  [(define (get-parent self)
     (error "Any-type does not have a parent"))

   (define (typeof-predicate self) Any-Type?)

   (define (is-supertype? self other-type)
     (Any-Type? other-type))

   (define (repr self)
     (list 'Any-type))

   (define (unify-helper self other-type mapping)
      (unless (and (not (term? other-type)) (Any-Type? other-type))
        (internal-error "unify-helper requirement not satisfied"))
      other-type)]

  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (case mode
       [(#t) (write (repr self) port)]
       [(#f) (display (repr self) port)]
       [else (print (repr self) port mode)]))])

(define (Any-type) (Any-Type))

(struct Bottom-Type Any-Type () #:transparent
  #:methods gen:Type
  [(define (get-parent self)
     (struct-copy Any-Type self))

   (define (typeof-predicate self) Bottom-Type?)

   (define (is-supertype? self other-type)
     (Bottom-Type? other-type))

   (define (repr self)
     (list 'Bottom-type))

   (define (unify-helper self other-type mapping)
      (unless (and (not (term? other-type)) (Bottom-Type? other-type))
        (internal-error "unify-helper requirement not satisfied"))
      other-type)])

(define (Bottom-type) (Bottom-Type))

(struct Alias-Type Any-Type (id base-type) #:transparent
  #:methods gen:Type
  [(define/generic gen-repr repr)

   (define (get-parent self)
     (struct-copy Any-Type self))

   (define (typeof-predicate self) Alias-Type?)

   (define (is-supertype? self other-type)
     (or (Bottom-Type? other-type)
         (and (Alias-Type? other-type)
              (equal? (Alias-Type-id self) (Alias-Type-id other-type)))))

   (define (repr self)
     (list 'Alias-type
           (Alias-Type-id self)
           (gen-repr (Alias-Type-base-type self))))

   (define (unify-helper self other-type mapping)
      (unless (and (not (term? other-type)) (Alias-Type? other-type))
        (internal-error "unify-helper requirement not satisfied"))
      (and (equal? (Alias-Type-id self) (Alias-Type-id other-type))
           other-type))]

  #:methods gen:symbolic
  [(define/generic gen-make-symbolic make-symbolic)

   (define (make-symbolic self varset)
     (gen-make-symbolic (Alias-Type-base-type self) varset))])

(define (Alias-type id base-type)
  (unless (and (symbol? id) (Type? base-type))
    (internal-error
     (format "Alias-type -- Invalid arguments ~a ~a" id base-type)))
  (Alias-Type id base-type))

(define-syntax-rule (define-type-alias name type)
  (define name (Alias-type 'name type)))

(struct Base-Type Any-Type (id) #:transparent
  #:methods gen:Type
  [(define (get-parent self)
     (struct-copy Any-Type self))

   (define (typeof-predicate self) Base-Type?)
   
   (define (is-supertype? self other-type)
     (or (Bottom-Type? other-type)
         (and (Base-Type? other-type)
              (= (Base-Type-id self) (Base-Type-id other-type)))))

   (define (repr self)
     (list 'Base-type (Base-Type-id self)))

   (define (unify-helper self other-type mapping)
      (unless (and (not (term? other-type)) (Base-Type? other-type))
        (internal-error "unify-helper requirement not satisfied"))
      (and (= (Base-Type-id self) (Base-Type-id other-type))
           other-type))]

  #:methods gen:symbolic
  [(define (make-symbolic self varset) #f)])

(define Base-type
  (let ([num-types 0])
    (lambda ()
      (begin0 (Base-Type num-types)
        (set! num-types (+ 1 num-types))))))

(define-syntax-rule (define-base-type name)
  (define name (Base-type)))

(struct Boolean-Type Any-Type () #:transparent
  #:methods gen:Type
  [(define (get-parent self)
     (struct-copy Any-Type self))

   (define (typeof-predicate self) Boolean-Type?)
   
   (define (is-supertype? self other-type)
     (or (Bottom-Type? other-type)
         (Boolean-Type? other-type)))

   (define (repr self)
     (list 'Boolean-type))

   (define (unify-helper self other-type mapping)
      (unless (and (not (term? other-type)) (Boolean-Type? other-type))
        (internal-error "unify-helper requirement not satisfied"))
      other-type)]

  #:methods gen:symbolic
  [(define (has-setters? self) #f)

   (define (make-symbolic self varset)
     (make-rosette-val boolean? varset))

   (define (generate-update-arg-names self update-type)
     (cond [(equal? update-type 'assign)
            (list (gensym 'bool-val))]

           [else
            (error (format "Unknown Boolean update type: ~a~%"
                           update-type))]))

   (define (update-code self update-type)
     (cond [(equal? update-type 'assign)
            (lambda (var val)
              #`(set! #,var #,val))]

           [else
            (error (format "Unknown Boolean update type: ~a~%"
                           update-type))]))

   (define (old-values-code self update-type var . update-args)
     (define old-val-tmp (gensym 'old-value))
     (cond [(equal? update-type 'assign)
            (list #`(define #,old-val-tmp #,var)
                    (list old-val-tmp)
                    (list self))]

           [else
            (error (format "Unknown Boolean update type: ~a~%"
                           update-type))]))

   (define (symbolic-update-code self update-type var update-args varset-name)
     (cond [(equal? update-type 'assign)
            (match-define (list val-tmp) update-args)
            (list (symbolic-code self val-tmp varset-name)
                  #`(set! #,var #,val-tmp)
                  (list self))]

           [else
            (error (format "Unknown Boolean update type: ~a~%"
                           update-type))]))])

(define (Boolean-type) (Boolean-Type))

;; TODO: Create Basic-Types, which have an id (a number) that identify
;; them. Allow the user to create new Basic-Types.
(struct Index-Type Any-Type () #:transparent
  #:methods gen:Type
  [(define (get-parent self)
     (struct-copy Any-Type self))

   (define (typeof-predicate self) Index-Type?)

   (define (is-supertype? self other-type)
     (or (Bottom-Type? other-type)
         (Index-Type? other-type)))

   (define (repr self)
     (list 'Index-type))

   (define (unify-helper self other-type mapping)
      (unless (and (not (term? other-type)) (Index-Type? other-type))
        (internal-error "unify-helper requirement not satisfied"))
      other-type)])

(define (Index-type) (Index-Type))

(struct Integer-Type Index-Type () #:transparent
  #:methods gen:Type
  [(define (get-parent self)
     (struct-copy Index-Type self))

   (define (typeof-predicate self) Integer-Type?)

   (define (is-supertype? self other-type)
     (or (Bottom-Type? other-type)
         (Integer-Type? other-type)))

   (define (repr self)
     (list 'Integer-type))

   (define (unify-helper self other-type mapping)
      (unless (and (not (term? other-type)) (Integer-Type? other-type))
        (internal-error "unify-helper requirement not satisfied"))
      other-type)]

  #:methods gen:symbolic
  [(define (has-setters? self) #f)

   (define (make-symbolic self varset)
     (make-rosette-val integer? varset))

   (define (generate-update-arg-names self update-type)
     (cond [(equal? update-type 'assign)
            (list (gensym 'int-val))]

           [(member update-type '(increment decrement))
            (list)]

           [else
            (error (format "Unknown Integer update type: ~a~%"
                           update-type))]))

   (define (update-code self update-type)
     (cond [(equal? update-type 'assign)
            (lambda (var val)
              #`(set! #,var #,val))]

           [(equal? update-type 'increment)
            (lambda (var)
              #`(set! #,var (+ #,var 1)))]

           [(equal? update-type 'decrement)
            (lambda (var)
              #`(set! #,var (- #,var 1)))]

           [else
            (error (format "Unknown Integer update type: ~a~%"
                           update-type))]))

   (define (old-values-code self update-type var . update-args)
     (define old-val-tmp (gensym 'old-value))
     (cond [(member update-type '(assign increment decrement))
            (list #`(define #,old-val-tmp #,var)
                    (list old-val-tmp)
                    (list self))]

           [else
            (error (format "Unknown Integer update type: ~a~%"
                           update-type))]))

   (define (symbolic-update-code self update-type var update-args varset-name)
     (cond [(equal? update-type 'assign)
            (match-define (list val-tmp) update-args)
            (list (symbolic-code self val-tmp varset-name)
                  #`(set! #,var #,val-tmp)
                  (list self))]

           [(equal? update-type 'increment)
            (assert (null? update-args))
            (list #'(void)
                    #`(set! #,var (+ #,var 1))
                    (list))]

           [(equal? update-type 'decrement)
            (assert (null? update-args))
            (list #'(void)
                    #`(set! #,var (- #,var 1))
                    (list))]

           [else
            (error (format "Unknown Integer update type: ~a~%"
                           update-type))]))])

(define (Integer-type) (Integer-Type))

(struct Bitvector-Type Index-Type (bits) #:transparent
  #:methods gen:Type
  [(define (get-parent self)
     (struct-copy Index-Type self))

   (define (typeof-predicate self) Bitvector-Type?)

   (define (is-supertype? self other-type)
     (or (Bottom-Type? other-type)
         (and (Bitvector-Type? other-type)
              (= (Bitvector-Type-bits self)
                 (Bitvector-Type-bits other-type)))))

   (define (repr self)
     (list 'Bitvector-type (Bitvector-Type-bits self)))

   (define (unify-helper self other-type mapping)
      (unless (and (not (term? other-type)) (Bitvector-Type? other-type))
        (internal-error "unify-helper requirement not satisfied"))
      (and (= (Bitvector-Type-bits self) (Bitvector-Type-bits other-type))
           other-type))]

  #:methods gen:symbolic
  [(define (has-setters? self) #f)

   (define (make-symbolic self varset)
     (make-rosette-val (bitvector (Bitvector-Type-bits self)) varset))

   (define (generate-update-arg-names self update-type)
     (cond [(equal? update-type 'assign)
            (list (gensym 'bv-val))]

           [(member update-type '(increment decrement))
            (list)]

           [else
            (error (format "Unknown Bitvector update type: ~a~%"
                           update-type))]))

   (define (update-code self update-type)
     (define bits (Bitvector-Type-bits self))
     (cond [(equal? update-type 'assign)
            (lambda (var val)
              #`(set! #,var #,val))]

           [(equal? update-type 'increment)
            (lambda (var)
              #`(set! #,var (bvadd #,var (bv 1 #,bits))))]

           [(equal? update-type 'decrement)
            (lambda (var)
              #`(set! #,var (bvsub #,var (bv 1 #,bits))))]

           [else
            (error (format "Unknown Bitvector update type: ~a~%"
                           update-type))]))

   (define (old-values-code self update-type var . update-args)
     (define old-val-tmp (gensym 'old-bv-value))
     (cond [(member update-type '(assign increment decrement))
            (list #`(define #,old-val-tmp #,var)
                    (list old-val-tmp)
                    (list self))]

           [else
            (error (format "Unknown Bitvector update type: ~a~%"
                           update-type))]))

   (define (symbolic-update-code self update-type var update-args varset-name)
     (define bits (Bitvector-Type-bits self))
     (cond [(equal? update-type 'assign)
            (match-define (list val-tmp) update-args)
            (list (symbolic-code self val-tmp varset-name)
                  #`(set! #,var #,val-tmp)
                  (list self))]

           [(equal? update-type 'increment)
            (assert (null? update-args))
            (list #'(void)
                  #`(set! #,var (bvadd #,var (bv 1 #,bits)))
                  (list))]

           [(equal? update-type 'decrement)
            (assert (null? update-args))
            (list #'(void)
                  #`(set! #,var (bvsub #,var (bv 1 #,bits)))
                  (list))]

           [else
            (error (format "Unknown Bitvector update type: ~a~%"
                           update-type))]))])

(define (Bitvector-type [bits (current-bitwidth)])
  (Bitvector-Type bits))

(struct Enum-Type Index-Type (name num-items)
  #:methods gen:Type
  [(define (get-parent self)
     (struct-copy Index-Type self))

   (define (typeof-predicate self) Enum-Type?)

   ;; If you have Word be (Enum-Type 3) and Topic be (Enum-Type 3), they are
   ;; still semantically different and Words should not replace
   ;; Topics. So, check subtyping by identity equality.
   ;; NOTE: This means that you cannot replace Topic with (Enum-Type 3),
   ;; as the latter will create a new Enum type which will be interpreted
   ;; as a completely new type.
   (define (is-supertype? self other-type)
     (or (Bottom-Type? other-type)
         (eq? self other-type)))

   (define (repr self)
     (Enum-Type-name self))

   (define (unify-helper self other-type mapping)
      (unless (and (not (term? other-type)) (Enum-Type? other-type))
        (internal-error "unify-helper requirement not satisfied"))
      (and (equal? (Enum-Type-name self) (Enum-Type-name other-type))
           other-type))]

  #:methods gen:symbolic
  [(define (has-setters? self) #f)

   (define (make-symbolic self varset)
     (make-bounded-val 0 (Enum-Type-num-items self) integer? varset))

   (define (generate-update-arg-names self update-type)
     (cond [(equal? update-type 'assign)
            (list (gensym (Enum-Type-name self)))]

           [else
            (error (format "Unknown Enum update type: ~a~%"
                           update-type))]))

   (define (update-code self update-type)
     (cond [(equal? update-type 'assign)
            (lambda (var val)
              #`(set! #,var #,val))]

           [else
            (error (format "Unknown Enum update type: ~a~%"
                           update-type))]))

   (define (old-values-code self update-type var . update-args)
     (cond [(equal? update-type 'assign)
            (define old-val-tmp (gensym 'old-value))
            (list #`(define #,old-val-tmp #,var)
                    (list old-val-tmp)
                    (list self))]

           [else
            (error (format "Unknown Enum update type: ~a~%"
                           update-type))]))

   (define (symbolic-update-code self update-type var update-args varset-name)
     (cond [(equal? update-type 'assign)
            (match-define (list val-tmp) update-args)
            (define num-items (Enum-Type-num-items self))
            (list (symbolic-code self val-tmp varset-name)
                  #`(set! #,var #,val-tmp)
                  (list self))]

           [else
            (error (format "Unknown Enum update type: ~a~%"
                           update-type))]))]
  
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     ((if mode write display)
      (Enum-Type-name self) port))])

(define (Enum-type name num-items)
  (Enum-Type name num-items))

(struct Vector-Type Any-Type (len index-type output-type) #:transparent
  #:methods gen:Type
  [(define/generic gen-is-supertype? is-supertype?)
   (define/generic gen-repr repr)
   (define/generic gen-get-free-type-vars get-free-type-vars)
   (define/generic gen-replace-type-vars replace-type-vars)
   
   (define (get-parent self)
     (struct-copy Any-Type self))

   (define (typeof-predicate self) Vector-Type?)

   ;; TODO: The output type has to be both co- and contra-variant
   (define (is-supertype? self other-type)
     (or (Bottom-Type? other-type)
         (and (Vector-Type? other-type)
              (gen-is-supertype? (Vector-Type-index-type other-type)
                                 (Vector-Type-index-type self))
              (gen-is-supertype? (Vector-Type-output-type self)
                                 (Vector-Type-output-type other-type)))))

   (define (repr self)
     (list 'Vector-type
           ;; The first argument is either the index type or the
           ;; vector length.
           (if (or (Enum-Type? (Vector-Type-index-type self))
                   (not (integer? (Vector-Type-len self))))
               (gen-repr (Vector-Type-index-type self))
               (Vector-Type-len self))
           (gen-repr (Vector-Type-output-type self))))

   (define (apply-on-symbolic-type-helper self fn)
     (for/all ([len (Vector-Type-len self)])
       (apply-on-symbolic-type
        (Vector-Type-index-type self)
        (lambda (c-index-type)
          (apply-on-symbolic-type
           (Vector-Type-output-type self)
           (lambda (c-output-type)
             (fn (Vector-Type len c-index-type c-output-type))))))))
   
   (define (unify-helper self other-type mapping)
     (unless (and (not (term? other-type)) (Vector-Type? other-type))
       (internal-error "unify-helper requirement not satisfied"))
     (define my-len (Vector-Type-len self))
     (define other-len (Vector-Type-len other-type))

     (define compatible-lengths?
       (not (and (integer? my-len) (integer? other-len)
                 (not (= my-len other-len)))))
           
     (define new-len
       (if (integer? my-len) my-len other-len))
     ;; For unification, we want to find a type that is
     ;; simultaneously self and other-type, so contravariance
     ;; does not apply. (Contravariance happens if you want to
     ;; find something that can be *substituted*.)
     (define new-index
       (and compatible-lengths?
            (unify (Vector-Type-index-type self)
                   (Vector-Type-index-type other-type)
                   mapping)))
     (define new-output
       (and new-index
            (unify (Vector-Type-output-type self)
                   (Vector-Type-output-type other-type)
                   mapping)))
     
     (and new-output (Vector-Type new-len new-index new-output)))

   (define (get-free-type-vars self)
     (append (gen-get-free-type-vars (Vector-Type-index-type self))
             (gen-get-free-type-vars (Vector-Type-output-type self))))

   (define (replace-type-vars self mapping [default #f])
     (Vector-Type (Vector-Type-len self)
                  (gen-replace-type-vars (Vector-Type-index-type self)
                                         mapping default)
                  (gen-replace-type-vars (Vector-Type-output-type self)
                                         mapping default)))]

  #:methods gen:symbolic
  [(define/generic gen-has-setters? has-setters?)
   (define/generic gen-make-symbolic make-symbolic)
   (define/generic gen-generate-update-arg-names generate-update-arg-names)
   (define/generic gen-update-code update-code)
   (define/generic gen-old-values-code old-values-code)
   (define/generic gen-symbolic-update-code symbolic-update-code)
   
   (define (has-setters? self) #t)
   
   (define (make-symbolic self varset)
     (unless (integer? (Vector-Type-len self))
       (error "Need integer length for vector -- make-symbolic"))

     (build-vector
      (Vector-Type-len self)
      (lambda (i)
        (gen-make-symbolic (Vector-Type-output-type self) varset))))

   (define (generate-update-arg-names self update-type)
     (define output-type (Vector-Type-output-type self))
     (cond [(equal? update-type 'assign)
            (list (gensym 'index) (gensym 'val))]

           [else
            (error (format "Unknown Vector update type: ~a~%"
                           update-type))]))

   (define (update-code self update-type)
     (define output-type (Vector-Type-output-type self))
     (cond [(equal? update-type 'assign)
            (lambda (vect index value)
              #`(vector-set! #,vect #,index #,value))]

           [else
            (error (format "Unknown Vector update type: ~a~%"
                           update-type))]))

   (define (old-values-code self update-type var . update-args)
     (define output-type (Vector-Type-output-type self))
     (cond [(equal? update-type 'assign)
            (let ([old-val-tmp (gensym 'old-value)])
              (list
               #`(define #,old-val-tmp (vector-ref #,var #,(car update-args)))
               (list old-val-tmp)
               (list output-type)))]
           
           [else
            (error (format "Unknown Vector update type: ~a~%"
                           update-type))]))

   (define (symbolic-update-code self update-type var update-args varset-name)
     (define len (Vector-Type-len self))
     (unless (integer? len)
       (error "Need integer length for vector -- symbolic-update-code"))
     
     (define output-type (Vector-Type-output-type self))
     (cond [(equal? update-type 'assign)
            (match-define (list tmp-index tmp-val) update-args)
            (list
             ;; Create the symbolic index into the vector
             #`(begin (define #,tmp-index
                        (make-symbolic (Enum-type 'vec-index #,len)
                                       #,varset-name))
                      ;; Create the symbolic value
                      #,(symbolic-code output-type tmp-val varset-name))
             ;; Perform the update
             #`(vector-set! #,var #,tmp-index #,tmp-val)
             (list (Vector-Type-index-type self) output-type))]

           [else
            (error (format "Unknown Vector update type: ~a~%"
                           update-type))]))])

;; TODO: Currently impossible to create a vector type with known
;; length but a type variable for an index type.
(define (Vector-type length-or-input output)
  (unless (and (or (Index-type? length-or-input)
                   (integer? length-or-input)
                   (and (Type-Var? length-or-input)
                        (Index-type? (Type-Var-default length-or-input))))
               (Type? output))
    (error (format "Invalid arguments to Vector-type: ~a and ~a~%"
                   length-or-input output)))
  
  (define length
    (cond [(Enum-Type? length-or-input)
           (Enum-Type-num-items length-or-input)]
          [(integer? length-or-input)
           length-or-input]
          [else 'unknown]))
  (define input
    (if (Type? length-or-input)
        length-or-input
        (Integer-type)))
  (Vector-Type length input output))

;; content-type must be an Enum type, Any-type, or a type variable
(struct Set-Type Any-Type (content-type) #:transparent
  #:methods gen:Type
  [(define/generic gen-is-supertype? is-supertype?)
   (define/generic gen-repr repr)
   (define/generic gen-get-free-type-vars get-free-type-vars)
   (define/generic gen-replace-type-vars replace-type-vars)
   
   (define (get-parent self)
     (struct-copy Any-Type self))

   (define (typeof-predicate self) Set-Type?)

   ;; TODO: The content type has to be both co- and contra-variant
   (define (is-supertype? self other-type)
     (or (Bottom-Type? other-type)
         (and (Set-Type? other-type)
              (gen-is-supertype? (Set-Type-content-type self)
                                 (Set-Type-content-type other-type)))))

   (define (repr self)
     (list 'Set-type (gen-repr (Set-Type-content-type self))))

   (define (apply-on-symbolic-type-helper self fn)
     (apply-on-symbolic-type
      (Set-Type-content-type self)
      (lambda (concrete-type) (fn (Set-Type concrete-type)))))

   (define (unify-helper self other-type mapping)
     (unless (and (not (term? other-type)) (Set-Type? other-type))
       (internal-error "unify-helper requirement not satisfied"))

     (let ([new-content (unify (Set-Type-content-type self)
                               (Set-Type-content-type other-type)
                               mapping)])
       (and new-content (Set-Type new-content))))

   (define (get-free-type-vars self)
     (gen-get-free-type-vars (Set-Type-content-type self)))
   
   (define (replace-type-vars self mapping [default #f])
     (Set-Type (gen-replace-type-vars (Set-Type-content-type self)
                                      mapping default)))]
  
  #:methods gen:symbolic
  [(define/generic gen-make-symbolic make-symbolic)
   (define/generic gen-generate-update-arg-names generate-update-arg-names)
   (define/generic gen-update-code update-code)
   (define/generic gen-old-values-code old-values-code)
   (define/generic gen-symbolic-update-code symbolic-update-code)
   
   (define (has-setters? self) #t)
   
   (define (make-symbolic self varset)
     (define num-items (Enum-Type-num-items (Set-Type-content-type self)))
     (enum-make-symbolic-set num-items varset))

   (define (generate-update-arg-names self update-type)
     (cond [(member update-type '(add remove))
            (list (gensym (Enum-Type-name (Set-Type-content-type self))))]

           [else
            (error (format "Unknown Set update type: ~a~%" update-type))]))

   (define (update-code self update-type)
     (cond [(member update-type '(add remove))
            (define update-name
              (if (equal? update-type 'add)
                  #'enum-set-add!
                  #'enum-set-remove!))
            (lambda (set value)
              #`(#,update-name #,set #,value))]

           [else
            (error (format "Unknown Set update type: ~a~%" update-type))]))

   (define (old-values-code self update-type var . update-args)
     (cond [(member update-type '(add remove))
            (let ([old-val-tmp (gensym 'was-in-set?)])
              (list #`(define #,old-val-tmp
                        (enum-set-contains? #,var #,(car update-args)))
                    (list old-val-tmp)
                    (list (Boolean-type))))]
           
           [else
            (error (format "Unknown Set update type: ~a~%" update-type))]))

   (define (symbolic-update-code self update-type var update-args varset-name)
     (cond [(member update-type '(add remove))
            (let ([tmp-val (car update-args)]
                  [update-name (if (equal? update-type 'add)
                                   #'enum-set-add!
                                   #'enum-set-remove!)]
                  [content-type (Set-Type-content-type self)])
              (list (symbolic-code content-type tmp-val varset-name)
                    ;; Perform the update
                    #`(#,update-name #,var #,tmp-val)
                    (list content-type)))]

           [else
            (error (format "Unknown Set update type: ~a~%" update-type))]))])

(define (Set-type content-type)
  (unless (or (Any-Type? content-type)
              (Enum-Type? content-type)
              (Type-Var? content-type))
    (error (format "Cannot make a Set-type containing ~a~%" content-type)))
  (Set-Type content-type))

(struct Map-Type Any-Type (capacity input-type output-type) #:transparent
  #:methods gen:Type
  [(define/generic gen-is-supertype? is-supertype?)
   (define/generic gen-repr repr)
   (define/generic gen-get-free-type-vars get-free-type-vars)
   (define/generic gen-replace-type-vars replace-type-vars)
   
   (define (get-parent self)
     (struct-copy Any-Type self))

   (define (typeof-predicate self) Map-Type?)

   ;; TODO: The output type has to be both co- and contra-variant
   (define (is-supertype? self other-type)
     (or (Bottom-Type? other-type)
         (and (Map-Type? other-type)
              (gen-is-supertype? (Map-Type-input-type other-type)
                                 (Map-Type-input-type self))
              (gen-is-supertype? (Map-Type-output-type self)
                                 (Map-Type-output-type other-type)))))

   (define (repr self)
     (list 'Map-type
           (Map-Type-capacity self)
           (gen-repr (Map-Type-input-type self))
           (gen-repr (Map-Type-output-type self))))

   (define (apply-on-symbolic-type-helper self fn)
     (for/all ([capacity (Map-Type-capacity self)])
       (apply-on-symbolic-type
        (Map-Type-input-type self)
        (lambda (c-input-type)
          (apply-on-symbolic-type
           (Map-Type-output-type self)
           (lambda (c-output-type)
             (fn (Map-Type capacity c-input-type c-output-type))))))))
   
   (define (unify-helper self other-type mapping)
     (unless (and (not (term? other-type)) (Map-Type? other-type))
       (internal-error "unify-helper requirement not satisfied"))

     (define my-capacity (Map-Type-capacity self))
     (define other-capacity (Map-Type-capacity other-type))
     (define new-capacity
       (if (integer? my-capacity) my-capacity other-capacity))

     (define new-input
       (unify (Map-Type-input-type self)
              (Map-Type-input-type other-type)
              mapping))
     (define new-output
       (and new-input
            (unify (Map-Type-output-type self)
                   (Map-Type-output-type other-type)
                   mapping)))
     
     (and new-output (Map-Type new-capacity new-input new-output)))

   (define (get-free-type-vars self)
     (append (gen-get-free-type-vars (Map-Type-input-type self))
             (gen-get-free-type-vars (Map-Type-output-type self))))

   (define (replace-type-vars self mapping [default #f])
     (Map-Type (Map-Type-capacity self)
               (gen-replace-type-vars (Map-Type-input-type self)
                                      mapping default)
               (gen-replace-type-vars (Map-Type-output-type self)
                                      mapping default)))]

  #:methods gen:symbolic
  [(define/generic gen-make-symbolic make-symbolic)
   
   (define (has-setters? self) #t)
   
   (define (make-symbolic self varset)
     (match self
       [(Map-Type capacity input-type output-type)
        (define (sym-input x) (gen-make-symbolic input-type varset))
        (define (sym-output x) (gen-make-symbolic output-type varset))
        (build-map capacity sym-input sym-output varset)]))])

(define (Map-type capacity input-type output-type)
  (unless (or (equal? capacity 'unknown)
              (and (integer? capacity) (not (term? capacity))))
    (error (format "Cannot make a Map-type with capacity ~a~%" capacity)))
  (Map-Type capacity input-type output-type))

;; vertex-type must be an Enum type, Any-type, or a type variable
(struct DAG-Type Any-Type (vertex-type) #:transparent
  #:methods gen:Type
  [(define/generic gen-is-supertype? is-supertype?)
   (define/generic gen-repr repr)
   (define/generic gen-get-free-type-vars get-free-type-vars)
   (define/generic gen-replace-type-vars replace-type-vars)
   
   (define (get-parent self)
     (struct-copy Any-Type self))

   (define (typeof-predicate self) DAG-Type?)

   ;; TODO: The vertex type has to be both co- and contra-variant
   (define (is-supertype? self other-type)
     (or (Bottom-Type? other-type)
         (and (DAG-Type? other-type)
              (gen-is-supertype? (DAG-Type-vertex-type self)
                                 (DAG-Type-vertex-type other-type)))))

   (define (repr self)
     (list 'DAG-type (gen-repr (DAG-Type-vertex-type self))))

   (define (apply-on-symbolic-type-helper self fn)
     (apply-on-symbolic-type
      (DAG-Type-vertex-type self)
      (lambda (concrete-type) (fn (DAG-Type concrete-type)))))

   (define (unify-helper self other-type mapping)
     (unless (and (not (term? other-type)) (DAG-Type? other-type))
       (internal-error "unify-helper requirement not satisfied"))

     (let ([new-vertex (unify (DAG-Type-vertex-type self)
                              (DAG-Type-vertex-type other-type)
                              mapping)])
       (and new-vertex (DAG-Type new-vertex))))

   (define (get-free-type-vars self)
     (gen-get-free-type-vars (DAG-Type-vertex-type self)))
   
   (define (replace-type-vars self mapping [default #f])
     (DAG-Type (gen-replace-type-vars (DAG-Type-vertex-type self)
                                      mapping default)))]
  
  #:methods gen:symbolic
  [(define/generic gen-has-setters? has-setters?)
   (define/generic gen-make-symbolic make-symbolic)
   (define/generic gen-generate-update-arg-names generate-update-arg-names)
   (define/generic gen-update-code update-code)
   (define/generic gen-old-values-code old-values-code)
   (define/generic gen-symbolic-update-code symbolic-update-code)
   
   (define (has-setters? self) #t)
   
   (define (make-symbolic self varset)
     (define size (Enum-Type-num-items (DAG-Type-vertex-type self)))
     (make-symbolic-graph size varset #:acyclic? #t))

   (define (generate-update-arg-names self update-type)
     (cond [(member update-type '(add-edge remove-edge))
            (define name (Enum-Type-name (DAG-Type-vertex-type self)))
            (list (gensym (string->symbol (format "~a-~a" name 'parent)))
                  (gensym (string->symbol (format "~a-~a" name 'child))))]

           [else
            (error (format "Unknown DAG update type: ~a~%" update-type))]))

   (define (update-code self update-type)
     (cond [(member update-type '(add-edge remove-edge))
            (define update-name
              (if (equal? update-type 'add-edge)
                  #'add-edge!
                  #'remove-edge!))
            (lambda (graph parent child)
              #`(#,update-name #,graph #,parent #,child))]

           [else
            (error (format "Unknown DAG update type: ~a~%" update-type))]))

   (define (old-values-code self update-type var . update-args)
     (cond [(member update-type '(add-edge remove-edge))
            (let ([old-val-tmp (gensym 'was-in-graph?)])
              (list #`(define #,old-val-tmp
                        (has-edge? #,var #,@update-args))
                    (list old-val-tmp)
                    (list (Boolean-type))))]
           
           [else
            (error (format "Unknown DAG update type: ~a~%" update-type))]))

   (define (symbolic-update-code self update-type var update-args varset-name)
     (cond [(member update-type '(add-edge remove-edge))
            (match update-args
              [(list parent-var child-var)
               (let ([update-name (if (equal? update-type 'add-edge)
                                      #'add-edge!
                                      #'remove-edge!)]
                     [vertex-type (DAG-Type-vertex-type self)])
                 (list
                  #`(begin
                      #,(symbolic-code vertex-type parent-var varset-name)
                      #,(symbolic-code vertex-type child-var varset-name))
                  ;; Perform the update
                  #`(#,update-name #,var #,parent-var #,child-var)
                  (list vertex-type vertex-type)))])]

           [else
            (error (format "Unknown DAG update type: ~a~%" update-type))]))])

(define (DAG-type vertex-type)
  (unless (or (Any-Type? vertex-type)
              (Enum-Type? vertex-type)
              (Type-Var? vertex-type))
    (error (format "Cannot make a DAG-type containing ~a~%" vertex-type)))
  (DAG-Type vertex-type))

;; A Record maps a fixed number of fields to values.
;; Fields must be known at compile time and cannot change.
;; So, a Record type should know all of the fields, and the types of
;; values that each field can contain.
(struct Record-Type Any-Type (constructor fields field-types field-constant?) #:transparent
  #:methods gen:Type
  [(define/generic gen-is-supertype? is-supertype?)
   (define/generic gen-repr repr)
   (define/generic gen-get-free-type-vars get-free-type-vars)
   (define/generic gen-replace-type-vars replace-type-vars)
   
   (define (get-parent self)
     (struct-copy Any-Type self))

   (define (typeof-predicate self) Record-Type?)

   (define (is-supertype? self other-type)
     (or (Bottom-Type? other-type)
         (and (Record-Type? other-type)
              (for/and ([id (Record-Type-fields self)])
                (and (member id (Record-Type-fields other-type))
                     (gen-is-supertype? (get-record-field-type self id)
                                        (get-record-field-type other-type id)))))))

   (define (repr self)
     (list 'Record-type
           (list 'quote (Record-Type-constructor self))
           (cons 'list (map (curry list 'quote) (Record-Type-fields self)))
           (cons 'list (map gen-repr (Record-Type-field-types self)))
           (cons 'list (Record-Type-field-constant? self))))

   (define (apply-on-symbolic-type-helper self fn)
     ;; TODO(correctness): for/all does not make fields and constant? concrete
     (for*/all ([constructor (Record-Type-constructor self)]
                [fields (Record-Type-fields self)]
                [constant? (Record-Type-field-constant? self)])
       (apply-on-symbolic-type-list
        (Record-Type-field-types self)
        (lambda (concrete-types)
          (fn (Record-Type constructor fields concrete-types constant?))))))

   ;; This unification is not commutative. It takes the name from
   ;; self and ignores the name of other-type. Same with constant?
   (define (unify-helper self other-type mapping)
     (when (union? self) (internal-error "Should not get a union here"))
     (unless (and (not (term? other-type)) (Record-Type? other-type))
       (internal-error "unify-helper requirement not satisfied"))

     (for/all ([other-type other-type])
       (match* (self other-type)
         [((Record-Type self-name self-fields self-types self-constant?)
           (Record-Type other-name other-fields other-types other-constant?))
          (for/all ([self-fields self-fields])
            (begin
              (define common-fields
                (for/list ([id self-fields]
                           #:when (member id other-fields))
                  id))
              (define self-common-types
                (map (curry lookup-field self-fields self-types)
                     common-fields))
              (define other-common-types
                (map (curry lookup-field other-fields other-types)
                     common-fields))
              (define common-types
                (for/list ([t1 self-common-types] [t2 other-common-types])
                  (unify t1 t2 mapping)))
              (Record-type self-name common-fields common-types self-constant?)))])))

   (define (get-free-type-vars self)
     (apply append
            (map gen-get-free-type-vars
                 (Record-Type-field-types self))))

   (define (replace-type-vars self mapping [default #f])
     (when (union? self) (internal-error "Should not get a union here"))
     (match self
       [(Record-Type constructor fields field-types field-constant?)
        (Record-type constructor fields
                     (for/all ([field-types field-types])
                       (for/list ([type field-types])
                         (gen-replace-type-vars type mapping default)))
                     field-constant?)]))]
  
  #:methods gen:symbolic
  [(define/generic gen-make-symbolic make-symbolic)
   (define/generic gen-generate-update-arg-names generate-update-arg-names)
   (define/generic gen-update-code update-code)
   (define/generic gen-old-values-code old-values-code)
   (define/generic gen-symbolic-update-code symbolic-update-code)
   
   (define (has-setters? self) #t)
   
   (define (make-symbolic self varset)
     (make-record (Record-Type-fields self)
                  (map (lambda (x) (gen-make-symbolic x varset))
                       (Record-Type-field-types self))))

   ;; TODO: We assume that the updates are of the form
   ;; record.field = value  // value is symbolically generated
   ;; We may also want built-in support for something like
   ;; record.field1.field2 = value
   (define (generate-update-arg-names self update-type)
     (if (member update-type (Record-Type-fields self))
         (list (gensym update-type))
         (error (format "Unknown Record field: ~a~%" update-type))))

   (define (update-code self update-type)
     (if (member update-type (Record-Type-fields self))
         (lambda (record value)
           #`(set-field #,record #,update-type #,value))
         (error (format "Unknown Record field: ~a~%" update-type))))

   (define (old-values-code self update-type var . update-args)
     (if (member update-type (Record-Type-fields self))
         (let ([old-val-tmp (gensym update-type)])
           (list #`(define #,old-val-tmp
                     (get-field #,(car update-args) #,update-type))
                 (list old-val-tmp)
                 (list (get-record-field-type self update-type))))
         (error (format "Unknown Record update type: ~a~%" update-type))))

   (define (symbolic-update-code self update-type var update-args varset-name)
     (if (member update-type (Record-Type-fields self))
         (match update-args
           [`(,record ,tmp-val)
            (let ([field-type (get-record-field-type self update-type)])
              (list (symbolic-code field-type tmp-val varset-name)
                    #`(set-field #,record #,update-type #,tmp-val)
                    (list field-type)))])
         (error (format "Unknown Record update type: ~a~%" update-type))))])

(define (get-record-field-type record field-name)
  (lookup-field (Record-Type-fields record)
                (Record-Type-field-types record)
                field-name))

(define (can-mutate-record-field? record field-name)
  (lookup-field (Record-Type-fields record)
                (Record-Type-field-constant? record)
                field-name))

(define (lookup-field fields attributes field-name)
  (for*/all ([fields fields] [attributes attributes])
    (for/first ([name fields] [attribute attributes]
                #:when (equal? name field-name))
      attribute)))
  

(define (Record-type constructor-name fields types [const? #f])
  ;; No matter what symbolic stuff we do, it should always be the
  ;; case that the fields are (possibly symbolic) Racket symbols.
  ;; Use internal-error instead of maybe-internal-error.
  (unless (andmap symbol? fields)
    (internal-error (format "Record fields must be symbols, but got ~a"
                            fields)))
  (unless (equal? fields (remove-duplicates fields))
    (maybe-internal-error
     (format "Records cannot have duplicate fields, given ~a" fields)))
  (Record-Type constructor-name fields types
               (or const? (build-list (length fields) (const #f)))))

(define-syntax (define-record stx)
  (define-splicing-syntax-class maybe-const
    ;; TODO: Better error messages (see above)
    (pattern (~seq #:const) #:with const? #'#t)
    (pattern (~seq) #:with const? #'#f))
  
  (syntax-parse stx
    [(_ name:id (field-name:id type:expr c:maybe-const) ...)
     (let ([num-fields (length (syntax-e #'(field-name ...)))])
       (with-syntax ([type-name (format-id #'name "~a-type" (syntax-e #'name))]
                     [num-stx (datum->syntax stx num-fields)])
         (syntax/loc stx
           (begin
             (define (name . args)
               (unless (= (length args) num-stx)
                 (error (format "Incorrect number of arguments to ~a: ~a expected, got ~a~%  Given arguments: ~a"
                                'name num-stx (length args) args)))
               (make-record '(field-name ...) args))

             (define type-name
               (Record-type 'name '(field-name ...)
                            (list type ...)
                            (list c.const? ...)))))))]))

;; Procedure types are complicated primarily because of how they
;; interact with type variables.
;; In particular, there is a difference between the following types:
;; forall T:     (T -> T') -> T -> T'
;; forall T, T': (T -> T') -> T -> T'
;; As a result, Procedure types need to store the type variables they
;; are quantified over. This should be sufficient, because
;; Hindley-Milner only allows top-level type quantification, and it
;; applies to procedures -- we don't need to do this for other types,
;; which are base types.
;; READ and WRITE procedures:
;; A read procedure is one that can be applied to a data structure.
;; If it is applied to a mutable data structure, then it will produce
;; a value that is allowed to be mutated. (Note that it might not make
;; sense to mutate it -- if you use vector-ref on a vector of ints,
;; you get an int, which you wouldn't want to mutate.)
;; A write procedure is one that mutates exactly one of its arguments.
;; write-index is the index into the domain that identifies which
;; argument to the procedure contains the data structure to mutate.
;; A procedure cannot be both a read and a write procedure.
;; domain-types:    List of types of arguments consumed.
;; range-type:      Type of value produced.
;; bound-type-vars: Set of type variables that are quantified here.
;; read-index:      0 <= read-index < (length domain-types) or #f.
;;                  If not #f, given (proc . args), the data structure
;;                  proc reads is (list-ref args read-index)
;; write-index:     0 <= write-index < (length domain-types) or #f.
;;                  If not #f, given (proc . args), the data structure
;;                  proc writes is (list-ref args write-index)
(struct Procedure-Type Any-Type (domain-types range-type bound-type-vars read-index write-index) #:transparent
  #:methods gen:Type
  [(define/generic gen-is-supertype? is-supertype?)
   (define/generic gen-repr repr)
   (define/generic gen-apply-on-symbolic-type-helper apply-on-symbolic-type-helper)
   (define/generic gen-get-free-type-vars get-free-type-vars)
   (define/generic gen-replace-type-vars replace-type-vars)

   (define (get-parent self)
     (struct-copy Any-Type self))

   (define (typeof-predicate self) Procedure-Type?)

   (define (is-supertype? self other-type)
     (or (Bottom-Type? other-type)
         (and (Procedure-Type? other-type)
              (let ([self-domain (Procedure-Type-domain-types self)]
                    [other-domain (Procedure-Type-domain-types other-type)]
                    [self-range (Procedure-Type-range-type self)]
                    [other-range (Procedure-Type-range-type other-type)])
                (and (= (length self-domain) (length other-domain))
                     (andmap gen-is-supertype? other-domain self-domain)
                     (gen-is-supertype? self-range other-range))))))

   ;; TODO: This will only work for user-defined Procedure types,
   ;; where the bound variables can be deduced from the domain and
   ;; range. If we ever construct Procedure types programmatically
   ;; that contain free variables, this will not work.
   (define (repr self)
     (when (union? self) (internal-error "Should not get a union here"))
     (match self
       [(Procedure-Type domain-types range-type _ ridx widx)
        (list 'Procedure-type
              (cons 'list (map gen-repr domain-types))
              (gen-repr range-type)
              '#:read-index ridx
              '#:write-index widx)]))

   (define (apply-on-symbolic-type-helper self fn)
     ;; TODO: Simplify using a macro
     (for/all ([domain-types (Procedure-Type-domain-types self)])
       (for/all ([range-type (Procedure-Type-range-type self)])
         (for/all ([bound-vars (Procedure-Type-bound-type-vars self)])
           (for/all ([ridx (Procedure-Type-read-index self)])
             (for/all ([widx (Procedure-Type-write-index self)])
                (apply-on-symbolic-type-list
                 domain-types
                 (lambda (cdts)
                   (gen-apply-on-symbolic-type-helper
                    range-type
                    (lambda (crt)
                      (apply-on-symbolic-type-list
                       bound-vars
                       (lambda (cbvs)
                         (fn (Procedure-Type cdts crt cbvs ridx widx))))))))))))))

   ;; TODO: What to do about read and write indexes?
   (define (unify-helper self other-type mapping)
     (unless (and (not (term? other-type)) (Procedure-Type? other-type))
       (internal-error "unify-helper requirement not satisfied"))

     (define copy (make-fresh self))
     (match* (copy other-type)
       [((Procedure-Type copy-domain copy-range _ copy-ridx copy-widx)
         (Procedure-Type other-domain other-range _ other-ridx other-widx))
        (and (= (length copy-domain) (length other-domain))
             ;; For unification, we want to find a type that is
             ;; simultaneously copy and other-type, so contravariance
             ;; does not apply. (Contravariance happens if you want to
             ;; find something that can be *substituted*.)
             (let ([new-domain
                    (map (lambda (x y) (unify x y mapping))
                         copy-domain other-domain)]
                   [new-range
                    (unify copy-range other-range mapping)])
               (and (andmap identity new-domain) new-range
                    (Procedure-type new-domain new-range
                                    #:read-index copy-ridx
                                    #:write-index copy-widx))))]))

   (define (get-free-type-vars self)
     (when (union? self) (internal-error "Should not get a union here"))
     
     (match self
       [(Procedure-Type domain range bound-vars _ _)
        (remove* (apply append (gen-get-free-type-vars range)
                        (map gen-get-free-type-vars domain))
                 bound-vars)]))

   ;; TODO: Handle procedure types with free variables
   (define (replace-type-vars self mapping [default #f])
     (when (union? self) (internal-error "Should not be a union"))
     (match self
       [(Procedure-Type domain range bound-vars ridx widx)
        (Procedure-type
         (map (lambda (x) (gen-replace-type-vars x mapping default)) domain)
         (gen-replace-type-vars range mapping default)
         #:read-index ridx
         #:write-index widx)]))])

(define (Procedure-type domain-types range-type
                        #:read-index [ridx #f]
                        #:write-index [widx #f])
  (when (and ridx widx)
    (error "A procedure cannot both read and write"))

  (define bound-vars
    (apply append (get-free-type-vars range-type)
           (map get-free-type-vars domain-types)))
  
  (Procedure-Type domain-types range-type bound-vars ridx widx))

;; Returns a copy of proc-type where all type variables are replaced
;; by new fresh type variables.
(define (make-fresh proc-type)
  (define mapping (make-type-map))
  (for/all ([bound-vars (Procedure-Type-bound-type-vars proc-type)])
    (for ([type-var bound-vars])
      (match type-var
        [(Type-Var id default)
         (add-type-binding! mapping type-var
                            (Type-Var (checked-gensym id) default))])))
  ;; Use #f as default so free type variables don't get replaced.
  (replace-type-vars proc-type mapping #f))

;; TODO: We could get unions of ids, what do we do in such cases?
(define (checked-gensym id)
  (when (union? id)
    (internal-error (format "Called gensym on a symbolic union: ~a" id)))
  (gensym id))

(define (apply-type proc-type arg-types)
  (define copy (make-fresh proc-type))  
  (for*/all ([copy copy]
             [arg-types arg-types])
    (let ([domain (Procedure-Type-domain-types copy)]
          [range (Procedure-Type-range-type copy)])
      (for/all ([domain domain])
        (and (= (length domain) (length arg-types))
             (let ([mapping (make-type-map)])
               (and (my-for/and ([expected-type domain]
                                 [actual-type arg-types])
                      (unify expected-type actual-type mapping))
                    ;; TODO: The result of replace-type-vars should
                    ;; not have any of the introduced fresh type
                    ;; variables. However it can have other type
                    ;; variables (such as ones in the arg-types).
                    (replace-type-vars range mapping))))))))

;; This can return types that contain free type variables.
(define (get-domain-given-range proc-type range-type [default #f])
  (define proc-copy (make-fresh proc-type))
  (let ([domain (Procedure-Type-domain-types proc-copy)]
        [range (Procedure-Type-range-type proc-copy)])
    (for/all ([domain domain])
      (begin
        (define mapping (make-type-map))
        (and (unify range range-type mapping)
             (map (lambda (domain-type)
                    (replace-type-vars domain-type mapping default))
                  domain))))))

(struct Error-Type Any-Type () #:transparent
  #:methods gen:Type
  [(define (get-parent self)
     (struct-copy Any-Type self))

   (define (typeof-predicate self) Error-Type?)

   (define (is-supertype? self other-type)
     (or (Bottom-Type? other-type)
         (Error-Type? other-type)))

   (define (repr self)
     (list 'Error-type))

   (define (unify-helper self other-type mapping)
     (unless (and (not (term? other-type)) (Error-Type? other-type))
       (internal-error "unify-helper requirement not satisfied"))
     other-type)])

(define (Error-type) (Error-Type))

(struct Void-Type Any-Type () #:transparent
  #:methods gen:Type
  [(define (get-parent self)
     (struct-copy Any-Type self))

   (define (typeof-predicate self) Void-Type?)

   (define (is-supertype? self other-type)
     (or (Bottom-Type? other-type)
         (Void-Type? other-type)))

   (define (repr self)
     (list 'Void-type))

   (define (unify-helper self other-type mapping)
     (unless (and (not (term? other-type)) (Void-Type? other-type))
       (internal-error "unify-helper requirement not satisfied"))
     other-type)])

(define (Void-type) (Void-Type))

;;;;;;;;;;;;;;;;;
;; Unification ;;
;;;;;;;;;;;;;;;;;

;; ADTs for unification -- type variables, and type binding maps
(struct Type-Var (id default) #:transparent
  #:methods gen:Type
  [(define/generic gen-repr repr)
   (define/generic gen-apply-on-symbolic-type-helper apply-on-symbolic-type-helper)
   (define/generic gen-replace-type-vars replace-type-vars)
   
   (define (get-parent self)
     (error "Cannot call get-parent on a type variable"))

   (define (typeof-predicate self) Type-Var?)

   (define (is-supertype? self other-type)
     (error "Cannot call is-supertype? on a type variable"))

   (define (repr self)
     `(Type-Var ',(Type-Var-id self) ,(gen-repr (Type-Var-default self))))

   (define (apply-on-symbolic-type-helper self fn)
     (for/all ([id (Type-Var-id self)])
       (for/all ([default (Type-Var-default self)])
         (gen-apply-on-symbolic-type-helper
          default
          (lambda (c-default)
            (fn (Type-Var id c-default)))))))
   
   (define (unify-helper self other-type mapping)
     (unless (not (term? other-type))
       (internal-error "unify-helper requirement not satisfied"))

     (if (equal? self other-type)
         other-type
         (and (add-type-binding! mapping self other-type)
              other-type)))

   (define (get-free-type-vars self)
     (list self))

   (define (replace-type-vars self mapping [default #f])
     (cond [(has-binding? mapping self)
            (gen-replace-type-vars (get-binding mapping self) mapping default)]
           [default (Type-Var-default self)]
           [else self]))]

  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (case mode
       [(#t) (write (Type-Var-id self) port)]
       [(#f) (display (Type-Var-id self) port)]
       [else (print (Type-Var-id self) port mode)]))]

  #:methods gen:equal+hash
  [(define (equal-proc x y recursive-equal?)
     (and (Type-Var? x) (Type-Var? y)
          (recursive-equal? (Type-Var-id x) (Type-Var-id y))))

   (define (hash-proc x recursive-equal-hash)
     (recursive-equal-hash (Type-Var-id x)))

   (define (hash2-proc x recursive-equal-hash)
     (recursive-equal-hash (Type-Var-id x)))])

(define (Type-var [default (Any-type)])
  (Type-Var (gensym 'alpha) default))

(define (make-type-map)
  (box (rhash)))

(define (has-binding? boxed-map type-var)
  (rhash-has-key? (unbox boxed-map) type-var))

(define (get-binding boxed-map type-var)
  (rhash-ref (unbox boxed-map) type-var))

;; Ensure that cycles don't develop
;; Returns #t on success, #f otherwise
;; This is essentially creating a disjoint set data structure
(define (add-type-binding! boxed-map type-var value)
  (define (make var val)
    (unless (Type-Var? var)
      (internal-error (format "Should be a type variable: ~a" var)))
    (set-box! boxed-map (rhash-set (unbox boxed-map) var val))
    #t)

  (define (get-final-value key)
    (if (has-binding? boxed-map key)
        (get-final-value (get-binding boxed-map key))
        key))

  (define orig-value (get-final-value type-var))
  (define new-value (get-final-value value))

  (cond [(eq? orig-value new-value) #t]
        [(Type-Var? orig-value) (make orig-value new-value)]
        [(Type-Var? new-value) (make new-value orig-value)]
        [else (unify orig-value new-value boxed-map)]))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mutability analysis ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(struct tm-pair (type mutable?) #:transparent)

;; It only makes sense for data structure types to be mutable
(define (make-tm-pair type mutable?)
  (tm-pair type (and mutable? (has-setters? type))))

(define (apply-type-with-mutability proc-type arg-pairs)
  (define new-range-type
    (apply-type proc-type (map tm-pair-type arg-pairs)))
  (define is-mutable?
    (compose tm-pair-mutable? (curry list-ref arg-pairs)))

  (let ([ridx (Procedure-Type-read-index proc-type)]
        [widx (Procedure-Type-write-index proc-type)])
    (and new-range-type
         ;; If this is a write procedure, it needs a mutable argument
         (or (not widx) (is-mutable? widx))
         ;; The range is mutable if this is a read procedure and the
         ;; read index argument is mutable
         (make-tm-pair new-range-type (and ridx (is-mutable? ridx))))))

;; The result of a procedure application can only be mutable if it
;; reads from a mutable data structure.
(define (is-application-mutable? proc-type args-mutable?)
  (let ([ridx (Procedure-Type-read-index proc-type)])
    (and ridx (list-ref args-mutable? ridx))))

;; Like get-domain-given-range, but includes constraints on
;; mutability.
;; range-pair is a tm-pair of the range type and range mutability.
(define (get-domain-given-range-with-mutability proc-type range-pair [default #f])
  (let ([domain-types
         (get-domain-given-range proc-type (tm-pair-type range-pair) default)])
    ;; Get domain types as before
    (and domain-types
         ;; By default, none of the arguments need to be mutable
         ;; handle-read-write will fix the result to account for Read
         ;; and Write procedures
         (handle-read-write
          proc-type (tm-pair-mutable? range-pair)
          (map (lambda (x) (make-tm-pair x #f)) domain-types)))))

;; Fixes results to account for Read and Write procedures.
(define (handle-read-write proc-type range-mutable? domain-pairs)
  (let ([ridx (Procedure-Type-read-index proc-type)]
        [widx (Procedure-Type-write-index proc-type)])
    (cond [widx
           ;; A Write procedure forces one of its arguments to be
           ;; mutable. It returns void which is never mutable.
           (and (not range-mutable?)
                (make-mutable domain-pairs widx))]
          ;; A Read procedure forces one of its arguments to be mutable
          ;; if it needs to produce a mutable result.
          [(and ridx range-mutable?)
           (make-mutable domain-pairs ridx)]
          [else
           ;; Since we aren't a read procedure, our return value is
           ;; not mutable.
           (and (not range-mutable?) domain-pairs)])))

;; Replaces the pair at the specified index with a new pair containing
;; the same type and the mutability flag set (i.e. #t)
(define (make-mutable type-mutable-pairs index)
  (if (= index 0)
      (cons (make-tm-pair (tm-pair-type (car type-mutable-pairs)) #t)
            (cdr type-mutable-pairs))
      (cons (car type-mutable-pairs)
            (make-mutable (cdr type-mutable-pairs) (- index 1)))))
