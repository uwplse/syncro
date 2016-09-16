#lang rosette

(require (only-in "constants.rkt" define-untyped-constant))

(provide Any-type Index-type Integer-type define-enum-type Vector-type
         Procedure-type Error-type Void-type Type? symbolic?
         ;; define-enum-type expands to (define var (Enum-Type ...)), so Enum-Type needs to be provided to rosette-namespace to avoid an unbound variable
         Enum-Type
         (rename-out [Vector-Type-index-type Vector-index-type]
                     [Vector-Type-output-type Vector-output-type])
         is-supertype? repr mutable-structure? symbolic-code
         update-code old-values-code symbolic-update-code
         Type-var unify apply-type
         (rename-out [Type? type?] [Any-Type? Any-type?] [Index-Type? Index-type?]
                     [Integer-Type? Integer-type?] [Enum-Type? Enum-type?]
                     [Vector-Type? Vector-type?] [Procedure-Type? Procedure-type?]
                     [Error-Type? Error-type?] [Void-Type? Void-type?]))

(define (set-add-code set-name val)
  (if set-name
      #`(set-add! #,set-name #,val)
      #'(void)))

;; TODO: Separate into two generic interfaces -- Type and symbolic
(define-generics Type
  (is-supertype? Type other-type)
  (repr Type)
  (unify Type other-type mapping)
  (replace-type-vars Type mapping)

  #:fallbacks
  [(define (unify self other-type mapping)
     (default-unify self other-type mapping))

   (define (replace-type-vars self mapping)
     self)])

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
  (mutable-structure? symbolic)
  (symbolic-code symbolic var [varset-name])
  (update-code symbolic update-type)
  (old-values-code symbolic update-type var . update-args)
  (symbolic-update-code symbolic update-type var))

(struct Any-Type ()
  #:methods gen:Type
  [(define (is-supertype? self other-type)
     (Any-Type? other-type))

   (define (repr self)
     '(Any-type))])

(define (Any-type) (Any-Type))

(struct Index-Type Any-Type ()
  #:methods gen:Type
  [(define (is-supertype? self other-type)
     (Index-Type? other-type))

   (define (repr self)
     '(Index-type))])

(define (Index-type) (Index-Type))

(struct Integer-Type Index-Type ()
  #:methods gen:Type
  [(define (is-supertype? self other-type)
     (Integer-Type? other-type))

   (define (repr self)
     '(Integer-type))]

  #:methods gen:symbolic
  [(define (mutable-structure? self) #f)

   (define (symbolic-code self var [varset-name #f])
     #`(begin (define-symbolic* #,var integer?)
              #,(set-add-code varset-name var)))

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
            (values #`(define #,old-val-tmp #,var)
                    (list old-val-tmp)
                    (list self))]

           [else
            (error (format "Unknown Integer update type: ~a~%"
                           update-type))]))

   (define (symbolic-update-code self update-type var)
     (cond [(equal? update-type 'assign)
            (define val-tmp (gensym 'val))
            (values #`(define-symbolic* #,val-tmp integer?)
                    #`(set! #,var #,val-tmp)
                    (list val-tmp)
                    (list self)
                    (list val-tmp))]

           [(equal? update-type 'increment)
            (values #'(void)
                    #`(set! #,var (+ #,var 1))
                    (list)
                    (list)
                    (list))]

           [(equal? update-type 'decrement)
            (values #'(void)
                    #`(set! #,var (- #,var 1))
                    (list)
                    (list)
                    (list))]

           [else
            (error (format "Unknown Integer update type: ~a~%"
                           update-type))]))])

(define (Integer-type) (Integer-Type))

(struct Enum-Type Index-Type (name num-items) #:transparent
  #:methods gen:Type
  [;; If you have Word be (Enum-Type% 3) and Topic be (Enum-Type% 3), they are
   ;; still semantically different and Words should not replace
   ;; Topics. So, check subtyping by identity equality.
   (define (is-supertype? self other-type)
     (eq? self other-type))

   (define (repr self)
     (Enum-Type-name self))]

  #:methods gen:symbolic
  [(define (mutable-structure? self) #f)

   (define (symbolic-code self var [varset-name #f])
     #`(begin (define-symbolic* #,var integer?)
              #,(set-add-code varset-name var)
              (assert (>= #,var 0))
              (assert (< #,var #,(Enum-Type-num-items self)))))

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
            (values #`(define #,old-val-tmp #,var)
                    (list old-val-tmp)
                    (list self))]

           [else
            (error (format "Unknown Enum update type: ~a~%"
                           update-type))]))

   (define (symbolic-update-code self update-type var)
     (cond [(equal? update-type 'assign)
            (define val-tmp (gensym 'val))
            (values #`(begin (define-symbolic* #,val-tmp integer?)
                             (assert (>= #,val-tmp 0))
                             (assert (< #,val-tmp #,(Enum-Type-num-items self))))
                    #`(set! #,var #,val-tmp)
                    (list val-tmp)
                    (list self)
                    (list val-tmp))]

           [else
            (error (format "Unknown Enum update type: ~a~%"
                           update-type))]))])

;; Different Enums should not be able to replace each other. So, in
;; the subtype method we compare Enums by identity. However if the
;; programmer keeps creating new types by saying (Enum-type 10) over
;; and over instead of creating a single type and using it
;; consistently, then we would get weird behavior.
;; To prevent this, Enum-type expands to a define, so as to force the
;; programmer to give the type a name. This makes it obvious that they
;; are supposed to reuse that name in the program.
(define-syntax-rule (define-enum-type name items)
  (define-untyped-constant name (Enum-Type 'name items)))

(struct Vector-Type Any-Type (len index-type output-type)
  #:methods gen:Type
  [(define/generic gen-is-supertype? is-supertype?)
   (define/generic gen-repr repr)
   (define/generic gen-unify unify)
   (define/generic gen-replace-type-vars replace-type-vars)
   
   (define (is-supertype? self other-type)
     (and (Vector-Type? other-type)
          (gen-is-supertype? (Vector-Type-index-type other-type)
                                 (Vector-Type-index-type self))
          (gen-is-supertype? (Vector-Type-output-type self)
                                 (Vector-Type-output-type other-type))))

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
                       ;; TODO: update-type needs to change here
                       update-type
                       #`(vector-ref #,var #,(car update-args))
                       (cdr update-args))

                (let ([old-val-tmp (gensym 'old-value)])
                  (values
                   #`(define #,old-val-tmp (vector-ref #,var #,(car update-args)))
                   (list old-val-tmp)
                   (list output-type))))]
           
           [else
            (error (format "Unknown Vector update type: ~a~%"
                           update-type))]))

   (define (symbolic-update-code self update-type var)
     (unless (integer? (Vector-Type-len self))
       (error "Need integer length for vector -- symbolic-update-code"))
     
     (define output-type (Vector-Type-output-type self))
     (cond [(and (equal? update-type 'assign)
                 (gen-mutable-structure? output-type))
            (define tmp-index (gensym 'index))
            (define tmp-val (gensym 'val))
            (let-values ([(output-defns output-update output-update-symbolic-vars output-update-symbolic-vars-types output-update-args)
                          (gen-symbolic-update-code
                           output-type
                           ;; TODO: update-type needs to change here
                           update-type
                           #`(vector-ref #,var #,tmp-index))])
              (values
               ;; Create the symbolic index into the vector
               #`(begin (define-symbolic* #,tmp-index integer?)
                        (assert (>= #,tmp-index 0))
                        (assert (< #,tmp-index #,(Vector-Type-len self)))
                        #,output-defns)
               ;; Update the mutable structure at the specified symbolic index
               output-update
               (cons tmp-index output-update-symbolic-vars)
               (cons (Vector-Type-index-type self) output-update-symbolic-vars-types)
               (cons tmp-index output-update-args)))]

           [(equal? update-type 'assign)
            (define tmp-index (gensym 'index))
            (define tmp-val (gensym 'val))
            (define old-val-tmp (gensym 'old-value))
            (values
             ;; Create the symbolic index into the vector
             #`(begin (define-symbolic* #,tmp-index integer?)
                      (assert (>= #,tmp-index 0))
                      (assert (< #,tmp-index #,(Vector-Type-len self)))
                      ;; Create the symbolic value
                      ;; TODO: This isn't an issue now, since output-type cannot be a mutable structure, but what if symbolic-code creates other symbolic variables besides tmp-val?
                      #,(gen-symbolic-code output-type tmp-val #f))
             ;; Perform the update
             #`(vector-set! #,var #,tmp-index #,tmp-val)
             (list tmp-index tmp-val)
             (list (Vector-Type-index-type self) output-type)
             (list tmp-index tmp-val))]

           [else
            (error (format "Unknown Vector update type: ~a~%"
                           update-type))]))])

;; TODO: Currently impossible to create a vector type with known
;; length but a type variable for an index type.
(define (Vector-type length-or-input output)
  (unless (and (or (Index-Type? length-or-input)
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

(struct Procedure-Type Any-Type (domain-types range-type)
  #:methods gen:Type
  [(define/generic gen-is-supertype? is-supertype?)
   (define/generic gen-repr repr)
   (define/generic gen-unify unify)
   (define/generic gen-replace-type-vars replace-type-vars)

   (define (is-supertype? self other-type)
     (and (Procedure-Type? other-type)
          (let ([self-domain (Procedure-Type-domain-types self)]
                [other-domain (Procedure-Type-domain-types other-type)]
                [self-range (Procedure-Type-range-type self)]
                [other-range (Procedure-Type-range-type other-type)])
            (and (= (length self-domain) (length other-domain))
                 (for/and ([self-dom-type self-domain]
                           [other-dom-type other-domain])
                   (gen-is-supertype? other-dom-type self-dom-type))
                 (gen-is-supertype? self-range other-range)))))

   (define (repr self)
     (let ([domain-types (Procedure-Type-domain-types self)]
           [range-type (Procedure-Type-range-type self)])
       `(Procedure-type (list ,@(map gen-repr domain-types))
                        (gen-repr range-type))))

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

(struct Error-Type Any-Type ()
  #:methods gen:Type
  [(define (is-supertype? self other-type)
     (Error-Type? other-type))

   (define (repr self)
     '(Error-type))])

(define (Error-type) (Error-Type))

(struct Void-Type Any-Type ()
  #:methods gen:Type
  [(define (is-supertype? self other-type)
     (Void-Type? other-type))

   (define (repr self)
     '(Void-type))])

(define (Void-type) (Void-Type))

;;;;;;;;;;;;;;;;;
;; Unification ;;
;;;;;;;;;;;;;;;;;

;; ADTs for unification -- type variables, and type binding maps
(struct Type-Var (id)
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
