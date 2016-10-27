#lang rosette

(provide Any-type Bottom-type Index-type Boolean-type Integer-type Enum-type
         Vector-type Procedure-type Error-type Void-type Type? symbolic?
         (rename-out [Vector-Type-index-type Vector-index-type]
                     [Vector-Type-output-type Vector-output-type])
         is-supertype? repr mutable-structure? symbolic-code
         generate-update-arg-names update-code old-values-code symbolic-update-code
         Type-var unify apply-type (rename-out [unify unify-types])
         type? Any-type? Bottom-type? Boolean-type? Index-type? Integer-type? Enum-type?
         Vector-type? Procedure-type? Error-type? Void-type?)

;; Creates type predicates that properly handle Bottom types.
(define-syntax (make-type-predicates stx)
  (syntax-case stx ()
    [(_ (pred sym) ...)
     (syntax/loc stx
       (begin (define (sym x)
                (or (Bottom-Type? x) (pred x)))
              ...))]))

(make-type-predicates
 [Type? type?] [Any-Type? Any-type?] [Bottom-Type? Bottom-type?]
 [Boolean-Type? Boolean-type?] [Index-Type? Index-type?]
 [Integer-Type? Integer-type?] [Enum-Type? Enum-type?]
 [Vector-Type? Vector-type?] [Procedure-Type? Procedure-type?]
 [Error-Type? Error-type?] [Void-Type? Void-type?] )

(define (set-add-code set-name val)
  (if set-name
      #`(set-add! #,set-name #,val)
      #'(void)))


(define-generics Type
  ;; Returns #t if type is a supertype of other-type, #f otherwise
  (is-supertype? Type other-type)
  ;; Returns an S-expression that could be used to create a new
  ;; instance of the type.
  (repr Type)
  ;; Unifies two types for type inference.
  ;; mapping is a type map that stores the unification bindings so far
  ;; (see bottom of this file).
  (unify Type other-type mapping)
  ;; Replaces type variables in this type with their values as given
  ;; by the type mapping.
  (replace-type-vars Type mapping)

  #:fallbacks
  [(define (unify self other-type mapping)
     (default-unify self other-type mapping))

   ;; Most types can never have type variables inside themselves
   (define (replace-type-vars self mapping)
     self)])

;; Unifies types assuming there are no type variables inside self.
;; other-type may be a type variable.
(define (default-unify self other-type mapping)
  (cond [(Type-Var? other-type)
         (add-type-binding! mapping other-type self)
         self]
        [(is-supertype? self other-type)
         other-type]
        [(is-supertype? other-type self)
         self]
        [else
         (error
          (format "Incompatible types for unification: ~a and ~a" self other-type))]))

(define-generics symbolic
  ;; Returns #t if it is possible to modify elements in a value of
  ;; this type (eg. Vectors), #f otherwise (eg. Booleans)
  (mutable-structure? symbolic)
  ;; Returns syntax that creates a symbolic value of this type
  ;; assigned to the variable var. The generated code also adds all
  ;; symbolic variables to varset-name, which is a symbol that at
  ;; runtime will have a set as a value. If varset-name is #f, that
  ;; code is not generated.
  (symbolic-code symbolic var [varset-name])
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
  (symbolic-update-code symbolic update-type var update-args))

(struct Any-Type () #:transparent
  #:methods gen:Type
  [(define (is-supertype? self other-type)
     (Any-Type? other-type))

   (define (repr self)
     '(Any-type))])

(define (Any-type) (Any-Type))

(struct Bottom-Type () #:transparent
  #:methods gen:Type
  [(define (is-supertype? self other-type)
     (Bottom-Type? other-type))

   (define (repr self)
     '(Bottom-type))])

(define (Bottom-type) (Bottom-Type))

(struct Boolean-Type Any-Type () #:transparent
  #:methods gen:Type
  [(define (is-supertype? self other-type)
     (or (Bottom-Type? other-type)
         (Boolean-Type? other-type)))

   (define (repr self)
     '(Boolean-type))]

  #:methods gen:symbolic
  [(define (mutable-structure? self) #f)

   (define (symbolic-code self var [varset-name #f])
     #`(begin (define-symbolic* #,var boolean?)
              #,(set-add-code varset-name var)))

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

   (define (symbolic-update-code self update-type var update-args)
     (cond [(equal? update-type 'assign)
            (match-define (list val-tmp) update-args)
            (list #`(define-symbolic* #,val-tmp boolean?)
                    #`(set! #,var #,val-tmp)
                    (list self))]

           [else
            (error (format "Unknown Boolean update type: ~a~%"
                           update-type))]))])

(define (Boolean-type) (Boolean-Type))

(struct Index-Type Any-Type () #:transparent
  #:methods gen:Type
  [(define (is-supertype? self other-type)
     (or (Bottom-Type? other-type)
         (Index-Type? other-type)))

   (define (repr self)
     '(Index-type))])

(define (Index-type) (Index-Type))

(struct Integer-Type Index-Type () #:transparent
  #:methods gen:Type
  [(define (is-supertype? self other-type)
     (or (Bottom-Type? other-type)
         (Integer-Type? other-type)))

   (define (repr self)
     '(Integer-type))]

  #:methods gen:symbolic
  [(define (mutable-structure? self) #f)

   (define (symbolic-code self var [varset-name #f])
     #`(begin (define-symbolic* #,var integer?)
              #,(set-add-code varset-name var)))

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

   (define (symbolic-update-code self update-type var update-args)
     (cond [(equal? update-type 'assign)
            (match-define (list val-tmp) update-args)
            (list #`(define-symbolic* #,val-tmp integer?)
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

(struct Enum-Type Index-Type (name num-items)
  #:methods gen:Type
  [;; If you have Word be (Enum-Type 3) and Topic be (Enum-Type 3), they are
   ;; still semantically different and Words should not replace
   ;; Topics. So, check subtyping by identity equality.
   ;; NOTE: This means that you cannot replace Topic with (Enum-Type% 3),
   ;; as the latter will create a new Enum type which will be interpreted
   ;; as a completely new type.
   (define (is-supertype? self other-type)
     (or (Bottom-Type? other-type)
         (eq? self other-type)))

   (define (repr self)
     (Enum-Type-name self))]

  #:methods gen:symbolic
  [(define (mutable-structure? self) #f)

   (define (symbolic-code self var [varset-name #f])
     #`(begin (define-symbolic* #,var integer?)
              #,(set-add-code varset-name var)
              (assert (>= #,var 0))
              (assert (< #,var #,(Enum-Type-num-items self)))))

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

   (define (symbolic-update-code self update-type var update-args)
     (cond [(equal? update-type 'assign)
            (match-define (list val-tmp) update-args)
            (list #`(begin (define-symbolic* #,val-tmp integer?)
                             (assert (>= #,val-tmp 0))
                             (assert (< #,val-tmp #,(Enum-Type-num-items self))))
                    #`(set! #,var #,val-tmp)
                    (list self))]

           [else
            (error (format "Unknown Enum update type: ~a~%"
                           update-type))]))])

(define (Enum-type name num-items)
  (Enum-Type name num-items))

(struct Vector-Type Any-Type (len index-type output-type) #:transparent
  #:methods gen:Type
  [(define/generic gen-is-supertype? is-supertype?)
   (define/generic gen-repr repr)
   (define/generic gen-unify unify)
   (define/generic gen-replace-type-vars replace-type-vars)
   
   (define (is-supertype? self other-type)
     (or (Bottom-Type? other-type)
         (and (Vector-Type? other-type)
              (gen-is-supertype? (Vector-Type-index-type other-type)
                                 (Vector-Type-index-type self))
              (gen-is-supertype? (Vector-Type-output-type self)
                                 (Vector-Type-output-type other-type)))))

   (define (repr self)
     (if (or (Enum-Type? (Vector-Type-index-type self))
             (not (integer? (Vector-Type-len self))))
         `(Vector-type ,(gen-repr (Vector-Type-index-type self))
                       ,(gen-repr (Vector-Type-output-type self)))
         `(Vector-type ,(Vector-Type-len self)
                       ,(gen-repr (Vector-Type-output-type self)))))

   (define (unify self other-type mapping)
     (if (not (Vector-Type? other-type))
         (default-unify self other-type mapping)
         (begin
           (define my-len (Vector-Type-len self))
           (define other-len (Vector-Type-len other-type))
           (when (and (integer? my-len) (integer? other-len)
                      (not (= my-len other-len)))
             (error "Incompatible lengths for vectors to be unified"))

           (define new-len
             (if (integer? my-len) my-len other-len))
           ;; For unification, we want to find a type that is
           ;; simultaneously self and other-type, so contravariance
           ;; does not apply. (Contravariance happens if you want to
           ;; find something that can be *substituted*.)
           (define new-index
             (gen-unify (Vector-Type-index-type self)
                        (Vector-Type-index-type other-type)
                        mapping))
           (define new-output
             (gen-unify (Vector-Type-output-type self)
                        (Vector-Type-output-type other-type)
                        mapping))
           (Vector-Type new-len new-index new-output))))

   (define (replace-type-vars self mapping)
     (Vector-Type (Vector-Type-len self)
                  (gen-replace-type-vars (Vector-Type-index-type self) mapping)
                  (gen-replace-type-vars (Vector-Type-output-type self) mapping)))]
             
  #:methods gen:symbolic
  [(define/generic gen-mutable-structure? mutable-structure?)
   (define/generic gen-symbolic-code symbolic-code)
   (define/generic gen-generate-update-arg-names generate-update-arg-names)
   (define/generic gen-update-code update-code)
   (define/generic gen-old-values-code old-values-code)
   (define/generic gen-symbolic-update-code symbolic-update-code)
   
   (define (mutable-structure? self) #t)
   
   (define (symbolic-code self var [varset-name #f])
     (unless (integer? (Vector-Type-len self))
       (error "Need integer length for vector -- symbolic-code"))
     
     (define tmp (gensym))
     #`(define #,var
         (build-vector
          #,(Vector-Type-len self)
          (lambda (i)
            #,(gen-symbolic-code (Vector-Type-output-type self) tmp varset-name)
            #,tmp))))

   (define (generate-update-arg-names self update-type)
     (define output-type (Vector-Type-output-type self))
     (cond [(equal? update-type 'assign)
            (if (gen-mutable-structure? output-type)
                (cons (gensym 'index)
                      (gen-generate-update-arg-names output-type update-type))
                (list (gensym 'index) (gensym 'val)))]

           [else
            (error (format "Unknown Vector update type: ~a~%"
                           update-type))]))

   (define (update-code self update-type)
     (define output-type (Vector-Type-output-type self))
     (cond [(equal? update-type 'assign)
            (if (gen-mutable-structure? output-type)

                (lambda (vect index . args)
                  (apply (gen-update-code output-type update-type)
                         #`(vector-ref #,vect #,index)
                         args))

                (lambda (vect index value)
                  #`(vector-set! #,vect #,index #,value)))]

           [else
            (error (format "Unknown Vector update type: ~a~%"
                           update-type))]))

   (define (old-values-code self update-type var . update-args)
     (define output-type (Vector-Type-output-type self))
     (cond [(equal? update-type 'assign)
            (if (gen-mutable-structure? output-type)
                (apply gen-old-values-code
                       output-type
                       update-type
                       #`(vector-ref #,var #,(car update-args))
                       (cdr update-args))

                (let ([old-val-tmp (gensym 'old-value)])
                  (list
                   #`(define #,old-val-tmp (vector-ref #,var #,(car update-args)))
                   (list old-val-tmp)
                   (list output-type))))]
           
           [else
            (error (format "Unknown Vector update type: ~a~%"
                           update-type))]))

   (define (symbolic-update-code self update-type var update-args)
     (unless (integer? (Vector-Type-len self))
       (error "Need integer length for vector -- symbolic-update-code"))
     
     (define output-type (Vector-Type-output-type self))
     (cond [(and (equal? update-type 'assign)
                 (gen-mutable-structure? output-type))
            (define tmp-index (car update-args))
            (match-let ([(list output-defns output-update output-update-symbolic-vars-types)
                          (gen-symbolic-update-code
                           output-type
                           update-type
                           #`(vector-ref #,var #,tmp-index)
                           (cdr update-args))])
              (list
               ;; Create the symbolic index into the vector
               #`(begin (define-symbolic* #,tmp-index integer?)
                        (assert (>= #,tmp-index 0))
                        (assert (< #,tmp-index #,(Vector-Type-len self)))
                        #,output-defns)
               ;; Update the mutable structure at the specified symbolic index
               output-update
               (cons (Vector-Type-index-type self) output-update-symbolic-vars-types)))]

           [(equal? update-type 'assign)
            (match-define (list tmp-index tmp-val) update-args)
            (define old-val-tmp (gensym 'old-value))
            (list
             ;; Create the symbolic index into the vector
             #`(begin (define-symbolic* #,tmp-index integer?)
                      (assert (>= #,tmp-index 0))
                      (assert (< #,tmp-index #,(Vector-Type-len self)))
                      ;; Create the symbolic value
                      ;; TODO: This isn't an issue now, since output-type cannot be a mutable structure, but what if symbolic-code creates other symbolic variables besides tmp-val?
                      #,(gen-symbolic-code output-type tmp-val #f))
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
                   (Type-Var? length-or-input)
                   (integer? length-or-input))
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

;; domain-types: List of types of arguments consumed.
;; range-type:   Type of value produced.
(struct Procedure-Type Any-Type (domain-types range-type) #:transparent
  #:methods gen:Type
  [(define/generic gen-is-supertype? is-supertype?)
   (define/generic gen-repr repr)
   (define/generic gen-unify unify)
   (define/generic gen-replace-type-vars replace-type-vars)

   (define (is-supertype? self other-type)
     (or (Bottom-Type? other-type)
         (and (Procedure-Type? other-type)
              (let ([self-domain (Procedure-Type-domain-types self)]
                    [other-domain (Procedure-Type-domain-types other-type)]
                    [self-range (Procedure-Type-range-type self)]
                    [other-range (Procedure-Type-range-type other-type)])
                (and (= (length self-domain) (length other-domain))
                     (for/and ([self-dom-type self-domain]
                               [other-dom-type other-domain])
                       (gen-is-supertype? other-dom-type self-dom-type))
                     (gen-is-supertype? self-range other-range))))))

   (define (repr self)
     (let ([domain-types (Procedure-Type-domain-types self)]
           [range-type (Procedure-Type-range-type self)])
       `(Procedure-type (list ,@(map gen-repr domain-types))
                        ,(gen-repr range-type))))

   (define (unify self other-type mapping)
     (if (not (Procedure-Type? other-type))
         (default-unify self other-type mapping)
         (begin
           (define self-domain (Procedure-Type-domain-types self))
           (define other-domain (Procedure-Type-domain-types other-type))
           (unless (= (length self-domain) (length other-domain))
             (error "Incompatible arities for procedures to be unified"))

           ;; For unification, we want to find a type that is
           ;; simultaneously self and other-type, so contravariance
           ;; does not apply. (Contravariance happens if you want to
           ;; find something that can be *substituted*.)
           (define new-domain
             (map (lambda (x y) (gen-unify x y mapping))
                  self-domain
                  other-domain))
           (define new-range
             (gen-unify (Procedure-Type-range-type self)
                        (Procedure-Type-range-type other-type)
                        mapping))
           (Procedure-type new-domain new-range))))

   (define (replace-type-vars self mapping)
     (Procedure-type (map (lambda (x) (gen-replace-type-vars x mapping))
                          (Procedure-Type-domain-types self))
                     (gen-replace-type-vars (Procedure-Type-range-type self) mapping)))])

(define (Procedure-type domain-types range-type)
  (Procedure-Type domain-types range-type))

;; TODO: Is it possible that type variables collide (that is, they are
;; forced to bind to the same type even though they could be allowed
;; to have different types)?
;; Potential solution: Copy proc-type here, with all type variables
;; being replaced by fresh type variables.
(define (apply-type proc-type arg-types)
  (let ([domain (Procedure-Type-domain-types proc-type)]
        [range (Procedure-Type-range-type proc-type)])
    (unless (= (length domain) (length arg-types))
      (error "Incorrect number of arguments -- apply-type"))
    
    (define mapping (make-type-map))
    (for ([expected-type domain]
          [actual-type arg-types])
      (unify expected-type actual-type mapping))

    (replace-type-vars range mapping)))

(struct Error-Type Any-Type () #:transparent
  #:methods gen:Type
  [(define (is-supertype? self other-type)
     (or (Bottom-Type? other-type)
         (Error-Type? other-type)))

   (define (repr self)
     '(Error-type))])

(define (Error-type) (Error-Type))

(struct Void-Type Any-Type () #:transparent
  #:methods gen:Type
  [(define (is-supertype? self other-type)
     (or (Bottom-Type? other-type)
         (Void-Type? other-type)))

   (define (repr self)
     '(Void-type))])

(define (Void-type) (Void-Type))

;;;;;;;;;;;;;;;;;
;; Unification ;;
;;;;;;;;;;;;;;;;;

;; ADTs for unification -- type variables, and type binding maps
(struct Type-Var (id) #:transparent
  #:methods gen:Type
  [(define/generic gen-replace-type-vars replace-type-vars)
   
   (define (is-supertype? self other-type)
     (error "Cannot call is-supertype? on a type variable"))

   (define (repr self)
     `(Type-Var ,(Type-Var-id self)))

   (define (unify self other-type mapping)
     (unless (equal? self other-type)
       (add-type-binding! mapping self other-type))
     other-type)

   (define (replace-type-vars self mapping)
     (if (hash-has-key? mapping self)
         (gen-replace-type-vars (hash-ref mapping self) mapping)
         self))]

  #:methods gen:equal+hash
  [(define (equal-proc x y recursive-equal?)
     (and (Type-Var? x) (Type-Var? y)
          (recursive-equal? (Type-Var-id x) (Type-Var-id y))))

   (define (hash-proc x recursive-equal-hash)
     (recursive-equal-hash (Type-Var-id x)))

   (define (hash2-proc x recursive-equal-hash)
     (recursive-equal-hash (Type-Var-id x)))])

(define (Type-var)
  (Type-Var (gensym 'alpha)))

(define (make-type-map)
  (make-hash))

(define (add-type-binding! mapping type-var value)
  ;; TODO: Should we check that type-var does not appear in value?
  ;; TODO: Even if we do, should we also check for cycles?
  ;; For example, alpha = (List beta), beta = (List alpha)
  (define new-value
    (if (hash-has-key? mapping type-var)
        ;; TODO: Is it a problem that we recursively call unify before
        ;; adding the constraint encoded by value?
        (unify value (hash-ref mapping type-var) mapping)
        value))
  (hash-set! mapping type-var new-value))