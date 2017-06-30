;; Lifting values enables synthesis where we can also recover the code
;; once synthesis is complete.
#lang rosette

(require "../enum-set.rkt" "../record.rkt"
         "../types.rkt" "../util.rkt" "../variable.rkt"
         racket/serialize)

(provide
 ;; Various constructs in the language
 define-lifted define-lifted-using-proc lifted?
 if^ begin^
 get-field^ set-field!^
 define^ set!^
 for-enum-set^
 lifted-error lifted-error?

 ;; Functions for lifted variables in particular
 lifted-variable? make-lifted-variable update-lifted-variable

 ;; Various selectors and predicates
 lifted-begin? lifted-begin-args lifted-define? lifted-define-var

 ;; Operations on lifted programs
 eval-lifted lifted-code fold-lifted infer-type mutable?
 force-type-helper eliminate-dead-code

 ;; Generic interfaces to extend the language
 lifted-writer gen:lifted gen:inferable

 ;; Deserialization procedures
 deserialize-lifted-variable deserialize-lifted-apply
 deserialize-lifted-begin deserialize-lifted-if
 deserialize-lifted-get-field deserialize-lifted-set-field!
 deserialize-lifted-define deserialize-lifted-set!
 deserialize-lifted-for-enum-set deserialize-lifted-error)

;; Since Rosette doesn't support objects, we'll use structs and
;; generic functions on those structs.

;; Serialization: We should be able to use serializable-struct, but it
;; doesn't work. So, for now we'll serialize just enough that
;; lifted-code works, since that's all we need.
;; TODO: Use serializable-struct once it is fixed.

;; TODO: Write a macro that will generate each of the structs?
;; Currently a lot of copy pasted code

(define-generics lifted
  ;; Evaluates the lifted expression to produce a value.
  (eval-lifted lifted)
  ;; Produces Racket code that represents the lifted expression.
  (lifted-code lifted)
  ;; Fold
  (fold-lifted lifted mapper reducer)

  #:defaults
  ([integer?
    (define (eval-lifted x) x)
    (define (lifted-code x) x)
    (define (fold-lifted x mapper reducer) (mapper x))]
   [boolean?
    (define (eval-lifted x) x)
    (define (lifted-code x) x)
    (define (fold-lifted x mapper reducer) (mapper x))]))

(define-generics inferable
  ;; Type inference
  (infer-type inferable)
  (force-type-helper inferable type mapping)
  ;; Mutability inference
  ;; Returns #t if you are allowed to mutate the return value, #f otherwise
  (mutable? inferable)
  #:defaults
  ([integer?
    (define (infer-type x) (Integer-type))
    (define (force-type-helper x type mapping)
      (assert-type type (Integer-type) mapping "integer"))
    (define (mutable? x) #f)]
   [boolean?
    (define (infer-type x) (Boolean-type))
    (define (force-type-helper x type mapping)
      (assert-type type (Boolean-type) mapping "boolean"))
    (define (mutable? x) #f)]
   [constant?  ;; A symbolic variable (but not a symbolic expression)
    (define (infer-type x)
      (let ([type (type-of x)])
        (cond [(equal? type integer?) (Integer-type)]
              [(equal? type boolean?) (Boolean-type)]
              [else
               (internal-error (format "Unsupported type: ~a~%" type))])))
    (define (force-type-helper x type mapping)
      (assert-type type (infer-type x) mapping "symbolic variable"))
    (define (mutable? x) #f)]))

(define (assert-type given-type spec-type mapping name)
  (unless (unify given-type spec-type mapping)
    (error (format "Invalid type for ~a: Requires ~a, got ~a under mapping ~a"
                   name spec-type given-type mapping)))) 

(define (apply-wrapper self . args)
  (if (ormap lifted-error? (cons self args))
      (lifted-error)
      ;; Forces Rosette not to merge nodes with different arities,
      ;; since that typically just causes a bunch of exceptions
      (match (length args)
        [0 (lifted-apply-0-args self args)]
        [1 (lifted-apply-1-arg  self args)]
        [2 (let ([op-name (variable-symbol self)]) 
          #;(lifted-apply-2-args self args)
          (cond
            [(equal? '+ op-name) (lifted-apply-arith-args self args)]
            [(equal? '- op-name) (lifted-apply-arith-args self args)]
            [(equal? '* op-name) (lifted-apply-arith-args self args)]
            [(equal? '< op-name) (lifted-apply-cmp-args self args)]
            [(equal? '= op-name) (lifted-apply-cmp-args self args)]
            [(equal? 'vector-increment! op-name) (lifted-apply-vecincdec-args self args)]
            [(equal? 'vector-decrement! op-name) (lifted-apply-vecincdec-args self args)]
            [(equal? 'vector-set! op-name) (lifted-apply-vecset-args self args)]
            [(equal? 'vector-ref op-name) (lifted-apply-vecref-args self args)]
            [(equal? 'enum-set-add! op-name) (lifted-apply-enum-set-modify-type-args self args)]
            [(equal? 'enum-set-remove! op-name) (lifted-apply-enum-set-modify-type-args self args)]
            [(equal? 'enum-set-contains? op-name) (lifted-apply-enum-set-contains?-type-args self args)]
            [(equal? 'map-ref op-name) (lifted-apply-map-ref-type-args self args)]
            [(equal? 'map-set! op-name) (lifted-apply-map-set!-type-args self args)]
            [(equal? 'add-edge! op-name) (lifted-apply-graph-modify-type-args self args)]
            [(equal? 'remove-edge! op-name) (lifted-apply-graph-modify-type-args self args)]
            [(equal? 'has-edge? op-name) (lifted-apply-graph-has-edge?-type-args self args)]
            [(equal? 'vertex-parent op-name) (lifted-apply-graph-get-set-type-args self args)]
            [(equal? 'vertex-children op-name) (lifted-apply-graph-get-set-type-args self args)]
            [else (internal-error (format "Unknown procedure: ~a" selftype))]))] ; missed some case
            ; Note: This will break for higher-order functions
        [_ (lifted-apply self args)])))


;; IMPORTANT: This is only a way to provide a custom write function to
;; many of the lifted constructs without having to rewrite it each
;; time. Not all lifted constructs extend lifted-writer, and so you
;; should NOT use lifted-writer? or similar things. Use lifted?
;; (defined by define-generics above).
(struct lifted-writer () #:transparent
  #:property prop:procedure apply-wrapper

  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (display "(lifted " port)
     (case mode
       [(#t) (write (lifted-code self) port)]
       [(#f) (display (lifted-code self) port)]
       [else (print (lifted-code self) port mode)])
     (display ")" port))]
)



(define deserialize-lifted-variable
  (make-deserialize-info
   (lambda (var)
     (lifted-variable var (Any-type) (unknown-value) #f #f))
   (const #f)))
;; Note that we depend on equality doing the right thing, which only
;; happens because of #:transparent
(struct lifted-variable variable () #:transparent
  #:property prop:procedure apply-wrapper
  #:property prop:serializable
  (make-serialize-info
   (lambda (s) (vector (variable-symbol s)))
   #'deserialize-lifted-variable
   #f
   (or (current-load-relative-directory) (current-directory)))
  
  #:methods gen:lifted
  [(define (eval-lifted self)
     (if (variable-has-value? self)
         (variable-value self)
         (error (format "Called eval-lifted on ~a before defining it"
                        (variable-symbol self)))))

   (define (lifted-code self)
     (variable-symbol self))

   (define (fold-lifted self mapper reducer)
     (mapper self))]

  #:methods gen:inferable
  [(define (infer-type self)
     (variable-type self))

   (define (force-type-helper self type mapping)
     (assert-type type (infer-type self) mapping (variable-symbol self)))

   (define (mutable? self)
     (variable-mutable? self))]

  #:methods gen:custom-write
  [(define (write-proc self port mode)
     ((if mode write display)
      (variable-symbol self) port))])

;; Almost verbatim from variable.rkt
(define (make-lifted-variable symbol type
                              #:value [value (unknown-value)]
                              #:mutable? [mutable? #f]
                              #:expression [expression #f])
  (lifted-variable symbol type value mutable? expression))

(define (update-lifted-variable var
                                #:symbol [symbol #f]
                                #:type [type #f]
                                #:value [value (unknown-value)]
                                #:mutable? [mutable? #f]
                                #:expression [expression #f])
  (unless (lifted-variable? var)
    (error (format "update-lifted-variable: Not a lifted variable: ~a" var)))
  (lifted-variable (or symbol (variable-symbol var))
                   (or type (variable-type var))
                   (if (unknown-value? value) (variable-value var) value)
                   (or mutable? (variable-mutable? var))
                   (or expression (variable-expression var))))



(define deserialize-lifted-apply
  (make-deserialize-info
   (lambda lst (apply lifted-apply lst))
   (const #f)))

(struct lifted-apply lifted-writer (proc args) #:transparent
  #:property prop:serializable
  (make-serialize-info
   (lambda (s) (vector (lifted-apply-proc s) (lifted-apply-args s)))
   #'deserialize-lifted-apply
   #f
   (or (current-load-relative-directory) (current-directory)))
  
  #:methods gen:lifted
  [(define/generic gen-eval-lifted eval-lifted)
   (define/generic gen-lifted-code lifted-code)
   (define/generic gen-fold-lifted fold-lifted)
   
   (define (eval-lifted self)
     (apply (gen-eval-lifted (lifted-apply-proc self))
            (map gen-eval-lifted (lifted-apply-args self))))

   (define (lifted-code self)
     (cons (gen-lifted-code (lifted-apply-proc self))
           (map gen-lifted-code (lifted-apply-args self))))

   (define (fold-lifted self mapper reducer)
     (apply reducer (mapper self)
            (gen-fold-lifted (lifted-apply-proc self) mapper reducer)
            (map (lambda (x) (gen-fold-lifted x mapper reducer))
                 (lifted-apply-args self))))]

  #:methods gen:inferable
  [(define/generic gen-infer-type infer-type)
   (define/generic gen-force-type-helper force-type-helper)
   (define/generic gen-mutable? mutable?)

   (define (infer-type self)
     (let ([proc-type (gen-infer-type (lifted-apply-proc self))]
           [arg-types (map gen-infer-type (lifted-apply-args self))])
       (or (apply-type proc-type arg-types)
           (error (format "Cannot apply ~a to arguments ~a"
                          proc-type arg-types)))))

   ;; TODO: Don't assume that we can call infer-type on the procedure
   ;; TODO: Maybe we should memoize calls to infer-type?
   (define (force-type-helper self type mapping)
     (let* ([proc-type (make-fresh (gen-infer-type (lifted-apply-proc self)))]
            [domain (Procedure-domain-types proc-type)]
            [range (Procedure-range-type proc-type)]
            [args (lifted-apply-args self)])
       (unless (= (length domain) (length args))
         (error "force-type: Incorrect number of arguments"))
       
       (assert-type type range mapping "procedure")
       (for-each (lambda (x y) (gen-force-type-helper x y mapping))
                 args domain)))

   (define (mutable? self)
     (is-application-mutable? (infer-type (lifted-apply-proc self))
                              (map gen-mutable? (lifted-apply-args self))))])

;; TODO: Are these serializable? Would be necessary for metasketches
(struct lifted-apply-0-args lifted-apply () #:transparent)
(struct lifted-apply-1-arg  lifted-apply () #:transparent)
(struct lifted-apply-2-args lifted-apply () #:transparent)
(struct lifted-apply-arith-args lifted-apply () #:transparent)
(struct lifted-apply-cmp-args lifted-apply () #:transparent)
(struct lifted-apply-andor-args lifted-apply () #:transparent)
(struct lifted-apply-vecincdec-args lifted-apply () #:transparent)
(struct lifted-apply-vecset-args lifted-apply () #:transparent)
(struct lifted-apply-vecref-args lifted-apply () #:transparent)
(struct lifted-apply-enum-set-modify-type-args lifted-apply () #:transparent)
(struct lifted-apply-enum-set-contains?-type-args lifted-apply () #:transparent)
(struct lifted-apply-map-ref-type-args lifted-apply () #:transparent)
(struct lifted-apply-map-set!-type-args lifted-apply () #:transparent)
(struct lifted-apply-graph-modify-type-args lifted-apply () #:transparent)
(struct lifted-apply-graph-has-edge?-type-args lifted-apply () #:transparent)
(struct lifted-apply-graph-get-set-type-args lifted-apply () #:transparent)

(define deserialize-lifted-begin
  (make-deserialize-info
   (lambda lst (apply lifted-begin lst))
   (const #f)))
(struct lifted-begin lifted-writer (args) #:transparent
  #:property prop:serializable
  (make-serialize-info
   (lambda (s) (vector (lifted-begin-args s)))
   #'deserialize-lifted-begin
   #f
   (or (current-load-relative-directory) (current-directory)))
  #:methods gen:lifted
  [(define/generic gen-eval-lifted eval-lifted)
   (define/generic gen-lifted-code lifted-code)
   (define/generic gen-fold-lifted fold-lifted)
   
   (define (eval-lifted self)
     ;; TODO: This would be an issue if the number of args is symbolic
     (for/last ([arg (lifted-begin-args self)])
       (gen-eval-lifted arg)))

   (define (lifted-code self)
     (cons 'let (cons '() (map gen-lifted-code (lifted-begin-args self)))))
   
   (define (fold-lifted self mapper reducer)
     (apply reducer (mapper self)
            (map (lambda (x) (gen-fold-lifted x mapper reducer))
                 (lifted-begin-args self))))]

  #:methods gen:inferable
  [(define/generic gen-infer-type infer-type)
   (define/generic gen-force-type-helper force-type-helper)
   (define/generic gen-mutable? mutable?)

   (define (infer-type self)
     (if (null? (lifted-begin-args self))
         (Void-type)
         (gen-infer-type (last (lifted-begin-args self)))))

   (define (force-type-helper self type mapping)
     (define last-index (length (lifted-begin-args self)))
     (for ([arg (lifted-begin-args self)]
           [i (in-naturals 1)])
       (gen-force-type-helper arg (if (= i last-index) type (Void-type))
                              mapping)))

   (define (mutable? self)
     (and (not (null? (lifted-begin-args self)))
          (gen-mutable? (last (lifted-begin-args self)))))])

(define (begin^ . args)
  (if (ormap lifted-error? args)
      (lifted-error)
      (lifted-begin args)))



(define deserialize-lifted-if
  (make-deserialize-info
   (lambda lst (apply lifted-if lst))
   (const #f)))
(struct lifted-if lifted-writer (condition then-branch else-branch) #:transparent
  #:property prop:serializable
  (make-serialize-info
   (lambda (s) (vector (lifted-if-condition s)
                       (lifted-if-then-branch s)
                       (lifted-if-else-branch s)))
   #'deserialize-lifted-if
   #f
   (or (current-load-relative-directory) (current-directory)))
  #:methods gen:lifted
  [(define/generic gen-eval-lifted eval-lifted)
   (define/generic gen-lifted-code lifted-code)
   (define/generic gen-fold-lifted fold-lifted)

   (define (eval-lifted self)
     (if (gen-eval-lifted (lifted-if-condition self))
         (gen-eval-lifted (lifted-if-then-branch self))
         (gen-eval-lifted (lifted-if-else-branch self))))

   (define (lifted-code self)
     (list 'if
           (gen-lifted-code (lifted-if-condition self))
           (gen-lifted-code (lifted-if-then-branch self))
           (gen-lifted-code (lifted-if-else-branch self))))
   
   (define (fold-lifted self mapper reducer)
     (reducer (mapper self)
              (gen-fold-lifted (lifted-if-condition self) mapper reducer)
              (gen-fold-lifted (lifted-if-then-branch self) mapper reducer)
              (gen-fold-lifted (lifted-if-else-branch self) mapper reducer)))]

  #:methods gen:inferable
  [(define/generic gen-infer-type infer-type)
   (define/generic gen-force-type-helper force-type-helper)
   (define/generic gen-mutable? mutable?)

   (define (infer-type self)
     (let ([ctype (gen-infer-type (lifted-if-condition self))]
           [ttype (gen-infer-type (lifted-if-then-branch self))]
           [etype (gen-infer-type (lifted-if-else-branch self))])
       (unless (Boolean-type? ctype)
         (error (format "If condition must be a boolean, got ~a" ctype)))
       (unify-types ttype etype)))

   (define (force-type-helper self type mapping)
     (gen-force-type-helper (lifted-if-condition self) (Boolean-type) mapping)
     (gen-force-type-helper (lifted-if-then-branch self) type mapping)
     (gen-force-type-helper (lifted-if-else-branch self) type mapping))

   (define (mutable? self)
     (and (gen-mutable? (lifted-if-then-branch self))
          (gen-mutable? (lifted-if-else-branch self))))])

(define (if^ t c e)
  (if (or (lifted-error? t) (lifted-error? c) (lifted-error? e))
      (lifted-error)
      (lifted-if t c e)))



(define deserialize-lifted-get-field
  (make-deserialize-info
   (lambda lst (apply lifted-get-field lst))
   (const #f)))
(struct lifted-get-field lifted-writer (record field-name) #:transparent
  #:property prop:serializable
  (make-serialize-info
   (lambda (s) (vector (lifted-get-field-record s)
                       (lifted-get-field-field-name s)))
   #'deserialize-lifted-get-field
   #f
   (or (current-load-relative-directory) (current-directory)))
  #:methods gen:lifted
  [(define/generic gen-eval-lifted eval-lifted)
   (define/generic gen-lifted-code lifted-code)
   (define/generic gen-fold-lifted fold-lifted)

   (define (eval-lifted self)
     (get-field (gen-eval-lifted (lifted-get-field-record self))
                (lifted-get-field-field-name self)))

   (define (lifted-code self)
     (list 'get-field
           (gen-lifted-code (lifted-get-field-record self))
           (list 'quote (lifted-get-field-field-name self))))
   
   (define (fold-lifted self mapper reducer)
     (reducer
      (mapper self)
      (gen-fold-lifted (lifted-get-field-record self) mapper reducer)))]

  #:methods gen:inferable
  [(define/generic gen-infer-type infer-type)
   (define/generic gen-force-type-helper force-type-helper)
   (define/generic gen-mutable? mutable?)

   (define (infer-type self)
     (let ([record-type (gen-infer-type (lifted-get-field-record self))])
       (unless (Record-type? record-type)
         (error (format "First argument to get-field must be a record, got ~a"
                        record-type)))
       (get-record-field-type record-type (lifted-get-field-field-name self))))

   (define (force-type-helper self type mapping)
     (when (union? self)
       (internal-error (format "get-field: Should not be a union: ~a" self)))
     (match self
       [(lifted-get-field record fname)
        (gen-force-type-helper record (Record-type (list fname) (list type))
                               mapping)]))

   (define (mutable? self)
     (gen-mutable? (lifted-get-field-record self)))])

(define (get-field^ record field-name)
  (unless (symbol? field-name)
    (internal-error
     (format "get-field^: Expected field name to be a symbol, got ~a"
             field-name)))
  (if (lifted-error? record)
      (lifted-error)
      (lifted-get-field record field-name)))



(define deserialize-lifted-set-field!
  (make-deserialize-info
   (lambda lst (apply lifted-set-field! lst))
   (const #f)))
(struct lifted-set-field! lifted-writer (record field-name value) #:transparent
  #:property prop:serializable
  (make-serialize-info
   (lambda (s) (vector (lifted-set-field!-record s)
                       (lifted-set-field!-field-name s)
                       (lifted-set-field!-value s)))
   #'deserialize-lifted-set-field!
   #f
   (or (current-load-relative-directory) (current-directory)))
  #:methods gen:lifted
  [(define/generic gen-eval-lifted eval-lifted)
   (define/generic gen-lifted-code lifted-code)
   (define/generic gen-fold-lifted fold-lifted)

   (define (eval-lifted self)
     (set-field! (gen-eval-lifted (lifted-set-field!-record self))
                 (lifted-set-field!-field-name self)
                 (gen-eval-lifted (lifted-set-field!-value self))))

   (define (lifted-code self)
     (list 'set-field!
           (gen-lifted-code (lifted-set-field!-record self))
           (list 'quote (lifted-set-field!-field-name self))
           (gen-lifted-code (lifted-set-field!-value self))))
   
   (define (fold-lifted self mapper reducer)
     (reducer
      (mapper self)
      (gen-fold-lifted (lifted-set-field!-record self) mapper reducer)
      (gen-fold-lifted (lifted-set-field!-value self) mapper reducer)))]

  #:methods gen:inferable
  [(define/generic gen-infer-type infer-type)
   (define/generic gen-force-type-helper force-type-helper)
   (define/generic gen-mutable? mutable?)

   (define (infer-type self)
     (let ([record-type (gen-infer-type (lifted-set-field!-record self))]
           [field-name (lifted-set-field!-field-name self)]
           [value-type (gen-infer-type (lifted-set-field!-value self))])
       (unless (Record-type? record-type)
         (error (format "First argument to set-field! must be a record, got ~a"
                        record-type)))
       (define expected-type (get-record-field-type record-type field-name))
       (unless (unify-types expected-type value-type)
         (error (format "Field ~a of ~a should have type ~a, but was ~a"
                        field-name record-type expected-type value-type)))
       (Void-type)))

   (define (force-type-helper self type mapping)
     (when (union? self)
       (internal-error (format "set-field!: Should not be a union: ~a" self)))

     (assert-type type (Void-type) mapping "set-field!")
     (match self
       [(lifted-set-field! record fname value)
        (let* ([fresh-var (Type-var)]
               [expected-type (Record-type (list fname) (list fresh-var))])
          (gen-force-type-helper record expected-type mapping)
          (gen-force-type-helper value fresh-var mapping))]))

   (define (mutable? self)
     #f)])

(define (set-field!^ record field-name value)
  (unless (symbol? field-name)
    (internal-error
     (format "set-field!^: Expected field name to be a symbol, got ~a"
             field-name)))
  (if (lifted-error? record)
      (lifted-error)
      (lifted-set-field! record field-name value)))



(define deserialize-lifted-define
  (make-deserialize-info
   (lambda lst (apply lifted-define lst))
   (const #f)))
;; var is a lifted-variable
;; val is a lifted expression
(struct lifted-define lifted-writer (var [val #:mutable]) #:transparent
  #:property prop:serializable
  (make-serialize-info
   (lambda (s) (vector (lifted-define-var s) (lifted-define-val s)))
   #'deserialize-lifted-define
   #f
   (or (current-load-relative-directory) (current-directory)))
  #:methods gen:lifted
  [(define/generic gen-eval-lifted eval-lifted)
   (define/generic gen-lifted-code lifted-code)
   (define/generic gen-fold-lifted fold-lifted)

   (define (eval-lifted self)
     (match self
       [(lifted-define var val)
        (let ([result (gen-eval-lifted val)])
          (set-variable-value! var result))]))

   (define (lifted-code self)
     (list 'define
           (variable-symbol (lifted-define-var self))
           (gen-lifted-code (lifted-define-val self))))

   (define (fold-lifted self mapper reducer)
     (reducer (mapper self)
              (gen-fold-lifted (lifted-define-val self) mapper reducer)))]

  #:methods gen:inferable
  [(define (infer-type self)
     (Void-type))

   (define (force-type-helper self type mapping)
     (assert-type type (Void-type) mapping "define"))

   (define (mutable? self) #f)])

;; var is a lifted variable
;; expr is a lifted expression
(define (define^ var val)
  (unless (and (lifted-variable? var) (lifted? val))
    (internal-error "define^: Expected lifted stuff"))
  (if (or (lifted-error? var) (lifted-error? val))
      (lifted-error)
      (lifted-define var val)))

;; var is syntax containing a symbol
;; expr is syntax containing an expression that evaluates to a lifted
;; expression
;; This currently is difficult to implement well. The syntax we want
;; is something like
;; (begin^ (define^ x 3)
;;         (*^ x 2))
;; However, this requires that x be defined to the lifted-variable
;; that is created for defines. This can't be done with a macro,
;; because define^ is in an expression context, so it can't expand to
;; defines. We could make begin^ also be a macro, and then arrange it
;; so that any define^s in the begin^ are evaluated in the surrounding
;; context (which could not be an expression context), and somehow
;; stitch together the results. However, then dynamically generating
;; define^s is more complicated -- it would have to be done with a
;; macro.
;; (define-syntax-rule (define^ var expr)
;;   (begin
;;     (define lifted-val expr)
;;     (define var
;;       (make-lifted-variable 'var (infer-type lifted-val)))
;;     (lifted-define var lifted-val)))



(define deserialize-lifted-set!
  (make-deserialize-info
   (lambda lst (apply lifted-set! lst))
   (const #f)))
(struct lifted-set! lifted-writer (var val) #:transparent
  #:property prop:serializable
  (make-serialize-info
   (lambda (s) (vector (lifted-set!-var s) (lifted-set!-val s)))
   #'deserialize-lifted-set!
   #f
   (or (current-load-relative-directory) (current-directory)))
  #:methods gen:lifted
  [(define/generic gen-eval-lifted eval-lifted)
   (define/generic gen-lifted-code lifted-code)
   (define/generic gen-fold-lifted fold-lifted)
   (define (eval-lifted self)
     (set-variable-value! (lifted-set!-var self)
                          (gen-eval-lifted (lifted-set!-val self)))
     (void))

   (define (lifted-code self)
     (list 'set!
           (gen-lifted-code (lifted-set!-var self))
           (gen-lifted-code (lifted-set!-val self))))

   (define (fold-lifted self mapper reducer)
     (reducer (mapper self)
              (gen-fold-lifted (lifted-set!-val self) mapper reducer)))]

  #:methods gen:inferable
  [(define (infer-type self)
     (Void-type))

   (define (force-type-helper self type mapping)
     (assert-type type (Void-type) mapping "set!"))

   (define (mutable? self) #f)])

(define (set!^ var val)
  (if (or (lifted-error? var) (lifted-error? val))
      (lifted-error)
      (lifted-set! var val)))



(define deserialize-lifted-for-enum-set
  (make-deserialize-info
   (lambda lst (apply lifted-for-enum-set lst))
   (const #f)))
(struct lifted-for-enum-set lifted-writer (var set-expr body) #:transparent
  #:property prop:serializable
  (make-serialize-info
   (lambda (s) (vector (lifted-for-enum-set-var s)
                       (lifted-for-enum-set-set-expr s)
                       (lifted-for-enum-set-body s)))
   #'deserialize-lifted-for-enum-set
   #f
   (or (current-load-relative-directory) (current-directory)))
  #:methods gen:lifted
  [(define/generic gen-eval-lifted eval-lifted)
   (define/generic gen-lifted-code lifted-code)
   (define/generic gen-fold-lifted fold-lifted)
   (define (eval-lifted self)
     (match self
       [(lifted-for-enum-set var set-expr body)
        (define set (gen-eval-lifted set-expr))
        (define num-items (vector-length set))

        (when (term? num-items)
          (internal-error
           (format "eval-lifted: Number of items in enum set should be concrete, was ~a"
                   num-items)))

        (for ([i num-items])
          (when (enum-set-contains? set i)
            (set-variable-value! var i)
            (gen-eval-lifted body)))

        (void)]))

   (define (lifted-code self)
     (match self
       [(lifted-for-enum-set var set-expr body)
        (list 'for-enum-set
              (list (list (gen-lifted-code var)
                          (gen-lifted-code set-expr)))
              (gen-lifted-code body))]))

   (define (fold-lifted self mapper reducer)
     (match self
       [(lifted-for-enum-set var set-expr body)
        (reducer (mapper self)
                 (gen-fold-lifted set-expr mapper reducer)
                 (gen-fold-lifted body mapper reducer))]))]

  #:methods gen:inferable
  [(define/generic gen-force-type-helper force-type-helper)
   (define (infer-type self)
     (Void-type))

   (define (force-type-helper self type mapping)
     (assert-type type (Void-type) mapping "for-enum-set")
     (match self
       [(lifted-for-enum-set var set-expr body)
        (let ([type-var (Type-var)])
          (gen-force-type-helper set-expr (Set-type type-var) mapping)
          (gen-force-type-helper var type-var mapping)
          (gen-force-type-helper body (Void-type) mapping))]))

   (define (mutable? self) #f)])

(define (for-enum-set^ var set-expr body)
  (if (or (lifted-error? var) (lifted-error? set-expr) (lifted-error? body))
      (lifted-error)
      (lifted-for-enum-set var set-expr body)))



(define deserialize-lifted-error
  (make-deserialize-info
   (lambda () (lifted-error))
   (const #f)))
(struct lifted-error lifted-writer () #:transparent
  #:property prop:serializable
  (make-serialize-info
   (lambda (s) (vector))
   #'deserialize-lifted-error
   #f
   (or (current-load-relative-directory) (current-directory)))
  #:methods gen:lifted
  [(define (eval-lifted self)
     (error "Default error -- LIFTED-ERROR"))

   (define (lifted-code self)
     '(error "Default error -- LIFTED-ERROR"))

   (define (fold-lifted self mapper reducer)
     (mapper self))]

  #:methods gen:inferable
  [(define (infer-type self)
     (Error-type))

   (define (force-type-helper self type mapping)
     (assert-type type (Error-type) mapping "error"))

   (define (mutable? self) #f)])



(define (lift value var-name type)
  (if (symbol? var-name)
      (make-lifted-variable var-name type #:value value)
      (error (format "Cannot lift ~a which has value ~a~%"
                     var-name value))))

;; Convenience macro to define many new lifted values at a time.
;; See grammar.rkt for an example.
(define-syntax (define-lifted stx)
  (syntax-case stx ()
    [(_ [thing new-name type] ...)
     (syntax/loc stx
       (define-lifted-using-proc [thing thing new-name type] ...))]))

(define-syntax (define-lifted-using-proc stx)
  (syntax-case stx ()
    [(_ [thing name-in-code new-name type] ...)
     (syntax/loc stx
       (begin (define new-name (lift thing 'name-in-code type))
              ...))]))

;; Removes any temporary variable definitions that are never used.
;; prederiv: List of lifted-defines.
;; postderiv: lifted-begin whose elements may be lifted-defines.
;; If postderiv is not a lifted-begin, we simply return everything
;; without performing any analysis.
;; Does not work on symbolic values.
(define (eliminate-dead-code prederiv postderiv)
  (if (not (and (list? prederiv) (lifted-begin? postderiv)))
      (values prederiv postderiv)
      (eliminate-dead-code-helper prederiv postderiv)))

(define (eliminate-dead-code-helper prederiv postderiv)
  (let ([sym-table (mutable-set)])
    (define (add-sym x)
      (when (lifted-variable? x)
        (set-add! sym-table (variable-symbol x))))

    ;; Ignore an item if it defines a temporary variable that is
    ;; never used.
    (define (ignore? item)
      (and (lifted-define? item)
           (not (set-member? sym-table
                             (variable-symbol (lifted-define-var item))))))

    (define (build-symbol-table item)
      (fold-lifted item add-sym (const #t)))

    (define (analyze-list lst)
      (reverse
       (for/list ([item (reverse lst)]
                  #:when (and (not (ignore? item))
                              (build-symbol-table item)))
         item)))

    (define updated-post
      (lifted-begin (analyze-list (lifted-begin-args postderiv))))
    (define updated-pre
      (analyze-list prederiv))
    (values updated-pre updated-post)))
