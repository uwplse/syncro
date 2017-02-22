;; Lifting values enables synthesis where we can also recover the code
;; once synthesis is complete.
#lang rosette

(require "../types.rkt" "../util.rkt" "../variable.rkt" racket/serialize)

(provide define-lifted lifted? if^ begin^ define-expr^ set!^
         eval-lifted lifted-code fold-lifted infer-type mutable?
         force-type-helper
         lifted-writer gen:lifted gen:inferable
         lifted-error lifted-error? make-lifted-variable
         deserialize-lifted-variable deserialize-lifted-apply
         deserialize-lifted-begin deserialize-lifted-if
         deserialize-lifted-define deserialize-lifted-set!
         deserialize-lifted-error)

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
  ([number?
    (define (eval-lifted x) x)
    (define (lifted-code x) x)
    (define (fold-lifted x mapper reducer) (mapper x))]))

(define-generics inferable
  ;; Type inference
  (infer-type inferable)
  (force-type-helper inferable type mapping)
  ;; Mutability inference
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
      (lifted-apply self args)))

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
     (display ")" port))])



(define deserialize-lifted-variable
  (make-deserialize-info
   (lambda (var)
     (lifted-variable var (Any-type) (unknown-value) #f #f))
   (const #f)))
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
                              #:definition [definition #f])
  (lifted-variable symbol type value mutable? definition))



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
     (apply-type (gen-infer-type (lifted-apply-proc self))
                 (map gen-infer-type (lifted-apply-args self))))

   ;; TODO: Don't assume that we can call infer-type on the procedure
   ;; TODO: Maybe we should memoize calls to infer-type?
   (define (force-type-helper self type mapping)
     (let ([proc-type (make-fresh (gen-infer-type (lifted-apply-proc self)))])
       (assert-type type (Procedure-range-type proc-type) mapping "procedure")
       (for-each (lambda (x y) (gen-force-type-helper x y mapping))
                 (lifted-apply-args self) (Procedure-domain-types proc-type))))

   (define (mutable? self)
     (is-application-mutable? (infer-type (lifted-apply-proc self))
                              (map gen-mutable? (lifted-apply-args self))))])



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
         (error "If condition must be a boolean, got" ctype))
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



(define deserialize-lifted-define
  (make-deserialize-info
   (lambda lst (apply lifted-define lst))
   (const #f)))
;; var is a lifted-variable
;; val is a lifted expression
(struct lifted-define lifted-writer (var val) #:transparent
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
     (set-variable-value! (lifted-define-var self)
                          (eval-lifted (lifted-define-val self))))

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

;; var is a symbol
;; expr is a lifted expression
(define (define-expr^ var lifted-val)
  (if (lifted-error? lifted-val)
      (lifted-error)
      (lifted-define
       (make-lifted-variable 'var (infer-type lifted-val))
       lifted-val)))

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
    [(define-lifted [thing new-name type] ...)
     (syntax/loc stx
       (begin (define new-name (lift thing 'thing type))
              ...))]))
