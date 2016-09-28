;; Lifting values enables synthesis where we can also recover the code
;; once synthesis is complete.
#lang s-exp rosette

(require "types.rkt")

(provide define-lifted lift lifted? if^ begin^ define^
         eval-lifted lifted-code infer-type
         ;; grammar.rkt defines Terminals, which are a subtype of lifted-variable
         lifted-variable lifted-variable-val lifted-variable-var lifted-variable-type
         lifted-error lifted-error?)

;; Lifting values enables synthesis where we can also recover the code
;; once synthesis is complete.

;; Since Rosette doesn't support objects, we'll use structs and
;; generic functions on those structs.

(define-generics lifted
  (eval-lifted lifted)
  (lifted-code lifted)

  #:defaults
  ([number?
    (define (eval-lifted x) x)
    (define (lifted-code x) x)]))

(define-generics inferable
  (infer-type inferable))

(struct lifted-variable (val var type) #:transparent
  #:methods gen:lifted
  [(define (eval-lifted self)
     (lifted-variable-val self))

   (define (lifted-code self)
     (lifted-variable-var self))]

  #:methods gen:inferable
  [(define (infer-type self)
     (lifted-variable-type self))])
     
(struct lifted-apply (proc args) #:transparent
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
     
(struct lifted-begin (args) #:transparent
  #:methods gen:lifted
  [(define/generic gen-eval-lifted eval-lifted)
   (define/generic gen-lifted-code lifted-code)
   
   (define (eval-lifted self)
     (for ([arg (lifted-begin-args self)])
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

(struct lifted-if (condition then-branch else-branch) #:transparent
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

(struct lifted-define (var val) #:transparent
  #:methods gen:lifted
  [(define/generic gen-lifted-code lifted-code)

   ;; TODO: Currently the terminal-info object acts as the
   ;; environment. We "do" the define before calling define^, so there
   ;; is nothing left to do here. In the future, may want to combine
   ;; the terminal-info into the lifted framework, and add terminals
   ;; simply by performing defines.
   (define (eval-lifted self)
     (void))

   (define (lifted-code self)
     (list 'define
           (lifted-define-var self)
           (gen-lifted-code (lifted-define-val self))))]

  #:methods gen:inferable
  [(define (infer-type self)
     (Void-type))])

(define (define^ var val)
  (lifted-define var val))

(struct lifted-error () #:transparent
  #:methods gen:lifted
  [(define (eval-lifted self)
     (error "Default error -- LIFTED-ERROR"))

   (define (lifted-code self)
     '(error "Default error -- LIFTED-ERROR"))]

  #:methods gen:inferable
  [(define (infer-type self)
     (Error-type))])

(define (lift value var-name type)
  (cond [(and (procedure? value) (symbol? var-name))
         (lambda arguments
           (lifted-apply (lifted-variable value var-name type) arguments))]
        [(symbol? var-name)
         (lifted-variable value var-name type)]
        [else
         (error (format "Cannot lift ~a which has value ~a~%"
                        var-name value))]))

(define-syntax (define-lifted stx)
  (syntax-case stx ()
    [(define-lifted [thing new-name type] ...)
     (syntax/loc stx
       (begin (define new-name (lift thing 'thing type))
              ...))]))
