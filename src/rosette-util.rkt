;; Lifting values enables synthesis where we can also recover the code
;; once synthesis is complete.
#lang rosette

(require "variable.rkt" "types.rkt")

(provide define-lifted lifted? if^ begin^ define-expr^ set!^
         eval-lifted lifted-code infer-type
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
  (infer-type inferable))



;; IMPORTANT: This is only a way to provide a custom write function to
;; many of the lifted constructs without having to rewrite it each
;; time. Not all lifted constructs extend lifted-writer, and so you
;; should NOT use lifted-writer? or similar things. Use lifted?
;; (defined by define-generics above).
(struct lifted-writer () #:transparent
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (define proc (if mode write display))
     (display "(lifted " port)
     (proc (lifted-code self) port)
     (display ")" port))])

(struct lifted-variable variable () #:transparent
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
     (variable-type self))]

  #:methods gen:custom-write
  [(define (write-proc self port mode)
     ((if mode write display)
      (variable-symbol self) port))])

;; Almost verbatim from variable.rkt
(define (make-lifted-variable symbol type
                              #:value [value (unknown-value)]
                              #:definition [definition #f]
                              . flags)
  (lifted-variable symbol type value definition (apply set flags)))

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

   (define (infer-type self)
     (apply-type (gen-infer-type (lifted-apply-proc self))
                 (map gen-infer-type (lifted-apply-args self))))])


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

   (define (infer-type self)
     (if (null? (lifted-begin-args self))
         (Void-type)
         (gen-infer-type (last (lifted-begin-args self)))))])

(define (begin^ . args)
  (lifted-begin args))


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

   (define (infer-type self)
     (let ([ctype (gen-infer-type (lifted-if-condition self))]
           [ttype (gen-infer-type (lifted-if-then-branch self))]
           [etype (gen-infer-type (lifted-if-else-branch self))])
       (unless (Boolean-type? ctype)
         (error "If condition must be a boolean, got" ctype))
       (unify-types ttype etype)))])

(define (if^ t c e)
  (lifted-if t c e))


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
     (Void-type))])

;; var is a symbol
;; expr is a lifted expression
(define (define-expr^ var lifted-val)
  (lifted-define
   (make-lifted-variable 'var (infer-type lifted-val))
   lifted-val))

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
  [(define/generic gen-lifted-code lifted-code)
   (define (eval-lifted self)
     (set-variable-value! (lifted-set!-var self)
                          (lifted-set!-val self))
     (void))

   (define (lifted-code self)
     (list 'set!
           (gen-lifted-code (lifted-set!-var self))
           (gen-lifted-code (lifted-set!-val self))))]

  #:methods gen:inferable
  [(define (infer-type self)
     (Void-type))])

(define (set!^ var val)
  (lifted-set! var val))


(struct lifted-error lifted-writer () #:transparent
  #:methods gen:lifted
  [(define (eval-lifted self)
     (error "Default error -- LIFTED-ERROR"))

   (define (lifted-code self)
     '(error "Default error -- LIFTED-ERROR"))]

  #:methods gen:inferable
  [(define (infer-type self)
     (Error-type))])


;; Despite the name, this does not always produce a lifted value.
;; When given a procedure, it produces a procedure that when called
;; will produce a lifted value. For anything else, it directly creates
;; a lifted value.
;; The goal here is that a program using lifted values looks exactly
;; the same, and such programs produces lifted value.
;; eg. (vector-ref^ my-vec^ 0) will produce a lifted value, although
;; vector-ref^ by itself will not.
;; TODO: We could fix this by making vector-ref^ a lifted variable
;; with a prop:procedure property?
(define (lift value var-name type)
  (cond [(and (procedure? value) (symbol? var-name))
         (lambda arguments
           (lifted-apply (make-lifted-variable var-name type #:value value) arguments))]
        [(symbol? var-name)
         (make-lifted-variable var-name type #:value value)]
        [else
         (error (format "Cannot lift ~a which has value ~a~%"
                        var-name value))]))

;; Convenience macro to define many new lifted values at a time.
;; See grammar.rkt for an example.
(define-syntax (define-lifted stx)
  (syntax-case stx ()
    [(define-lifted [thing new-name type] ...)
     (syntax/loc stx
       (begin (define new-name (lift thing 'thing type))
              ...))]))
