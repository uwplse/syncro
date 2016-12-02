;; Lifting values enables synthesis where we can also recover the code
;; once synthesis is complete.
#lang rosette

(require "variable.rkt" "types.rkt" "util.rkt")

(provide define-lifted lifted? if^ begin^ define-expr^ set!^
         eval-lifted lifted-code infer-type mutable? lifted-writer
         gen:lifted gen:inferable
         lifted-error lifted-error?
         make-lifted-variable)

;; Since Rosette doesn't support objects, we'll use structs and
;; generic functions on those structs.

(define-generics lifted
  ;; Evaluates the lifted expression to produce a value.
  (eval-lifted lifted)
  ;; Produces Racket code that represents the lifted expression.
  (lifted-code lifted)

  #:defaults
  ([number?
    (define (eval-lifted x) x)
    (define (lifted-code x) x)]))

(define-generics inferable
  ;; Type inference
  (infer-type inferable)
  ;; Mutability inference
  ;; TODO: Is this even necessary? I don't think it's dead code right now
  (mutable? inferable)
  #:defaults
  ([integer?
    (define (infer-type x) (Integer-type))
    (define (mutable? x) #f)]
   [boolean?
    (define (infer-type x) (Boolean-type))
    (define (mutable? x) #f)]
   [constant?  ;; A symbolic variable (but not a symbolic expression)
    (define (infer-type x)
      (let ([type (type-of x)])
        (cond [(equal? type integer?) (Integer-type)]
              [(equal? type boolean?) (Boolean-type)]
              [else
               (internal-error (format "Unsupported type: ~a~%" type))])))
    (define (mutable? x) #f)]))


;; IMPORTANT: This is only a way to provide a custom write function to
;; many of the lifted constructs without having to rewrite it each
;; time. Not all lifted constructs extend lifted-writer, and so you
;; should NOT use lifted-writer? or similar things. Use lifted?
;; (defined by define-generics above).
(struct lifted-writer () #:transparent
  #:property prop:procedure
  (lambda (self . args) (lifted-apply self args))
  
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (display "(lifted " port)
     (case mode
       [(#t) (write (lifted-code self) port)]
       [(#f) (display (lifted-code self) port)]
       [else (print (lifted-code self) port mode)])
     (display ")" port))])

(struct lifted-variable variable () #:transparent
  #:property prop:procedure
  (lambda (self . args)
    (if (ormap lifted-error? args)
        (lifted-error)
        (lifted-apply self args)))
  
  #:methods gen:lifted
  [(define (eval-lifted self)
     (if (variable-has-value? self)
         (variable-value self)
         (error (format "Called eval-lifted on ~a before defining it"
                        (variable-symbol self)))))

   (define (lifted-code self)
     (variable-symbol self))]

  #:methods gen:inferable
  [(define (infer-type self)
     (variable-type self))

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
                              #:definition [definition #f]
                              . flags)
  (lifted-variable symbol type value mutable? definition))

(struct lifted-apply lifted-writer (proc args) #:transparent
  #:methods gen:lifted
  [(define/generic gen-eval-lifted eval-lifted)
   (define/generic gen-lifted-code lifted-code)
   
   (define (eval-lifted self)
     (apply (gen-eval-lifted (lifted-apply-proc self))
            (map gen-eval-lifted (lifted-apply-args self))))

   (define (lifted-code self)
     (cons (gen-lifted-code (lifted-apply-proc self))
           (map gen-lifted-code (lifted-apply-args self))))]

  #:methods gen:inferable
  [(define/generic gen-infer-type infer-type)
   (define/generic gen-mutable? mutable?)

   (define (infer-type self)
     (apply-type (gen-infer-type (lifted-apply-proc self))
                 (map gen-infer-type (lifted-apply-args self))))

   (define (mutable? self)
     (is-application-mutable? (infer-type (lifted-apply-proc self))
                              (map gen-mutable? (lifted-apply-args self))))])


(struct lifted-begin lifted-writer (args) #:transparent
  #:methods gen:lifted
  [(define/generic gen-eval-lifted eval-lifted)
   (define/generic gen-lifted-code lifted-code)
   
   (define (eval-lifted self)
     (for/last ([arg (lifted-begin-args self)])
       (gen-eval-lifted arg)))

   (define (lifted-code self)
     (cons 'let (cons '() (map gen-lifted-code (lifted-begin-args self)))))]

  #:methods gen:inferable
  [(define/generic gen-infer-type infer-type)
   (define/generic gen-mutable? mutable?)

   (define (infer-type self)
     (if (null? (lifted-begin-args self))
         (Void-type)
         (gen-infer-type (last (lifted-begin-args self)))))

   (define (mutable? self)
     (and (not (null? (lifted-begin-args self)))
          (gen-mutable? (last (lifted-begin-args self)))))])

(define (begin^ . args)
  (if (ormap lifted-error? args)
      (lifted-error)
      (lifted-begin args)))


(struct lifted-if lifted-writer (condition then-branch else-branch) #:transparent
  #:methods gen:lifted
  [(define/generic gen-eval-lifted eval-lifted)
   (define/generic gen-lifted-code lifted-code)

   (define (eval-lifted self)
     (if (gen-eval-lifted (lifted-if-condition self))
         (gen-eval-lifted (lifted-if-then-branch self))
         (gen-eval-lifted (lifted-if-else-branch self))))

   (define (lifted-code self)
     (list 'if
           (gen-lifted-code (lifted-if-condition self))
           (gen-lifted-code (lifted-if-then-branch self))
           (gen-lifted-code (lifted-if-else-branch self))))]

  #:methods gen:inferable
  [(define/generic gen-infer-type infer-type)
   (define/generic gen-mutable? mutable?)

   (define (infer-type self)
     (let ([ctype (gen-infer-type (lifted-if-condition self))]
           [ttype (gen-infer-type (lifted-if-then-branch self))]
           [etype (gen-infer-type (lifted-if-else-branch self))])
       (unless (Boolean-type? ctype)
         (error "If condition must be a boolean, got" ctype))
       (unify-types ttype etype)))

   (define (mutable? self)
     (and (gen-mutable? (lifted-if-then-branch self))
          (gen-mutable? (lifted-if-else-branch self))))])

(define (if^ t c e)
  (if (or (lifted-error? t) (lifted-error? c) (lifted-error? e))
      (lifted-error)
      (lifted-if t c e)))


;; var is a lifted-variable
;; val is a lifted expression
(struct lifted-define lifted-writer (var val) #:transparent
  #:methods gen:lifted
  [(define/generic gen-eval-lifted eval-lifted)
   (define/generic gen-lifted-code lifted-code)

   (define (eval-lifted self)
     (set-variable-value! (lifted-define-var self)
                          (eval-lifted (lifted-define-val self))))

   (define (lifted-code self)
     (list 'define
           (variable-symbol (lifted-define-var self))
           (gen-lifted-code (lifted-define-val self))))]

  #:methods gen:inferable
  [(define (infer-type self)
     (Void-type))

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


(struct lifted-set! lifted-writer (var val) #:transparent
  #:methods gen:lifted
  [(define/generic gen-eval-lifted eval-lifted)
   (define/generic gen-lifted-code lifted-code)
   (define (eval-lifted self)
     (set-variable-value! (lifted-set!-var self)
                          (gen-eval-lifted (lifted-set!-val self)))
     (void))

   (define (lifted-code self)
     (list 'set!
           (gen-lifted-code (lifted-set!-var self))
           (gen-lifted-code (lifted-set!-val self))))]

  #:methods gen:inferable
  [(define (infer-type self)
     (Void-type))

   (define (mutable? self) #f)])

(define (set!^ var val)
  (if (or (lifted-error? var) (lifted-error? val))
      (lifted-error)
      (lifted-set! var val)))


(struct lifted-error lifted-writer () #:transparent
  #:methods gen:lifted
  [(define (eval-lifted self)
     (error "Default error -- LIFTED-ERROR"))

   (define (lifted-code self)
     '(error "Default error -- LIFTED-ERROR"))]

  #:methods gen:inferable
  [(define (infer-type self)
     (Error-type))

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
