;; Lifting values enables synthesis where we can also recover the code
;; once synthesis is complete.
#lang s-exp rosette

(provide define-lifted lift lifted? begin^
         eval-lifted lifted-code
         ;; grammar.rkt defines Terminals, which are a subtype of lifted-variable
         lifted-variable lifted-variable-val lifted-variable-var ; lifted-variable-type
         lifted-error)

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

(struct lifted-variable (val var) #:transparent
  #:methods gen:lifted
  [(define (eval-lifted self)
     (lifted-variable-val self))

   (define (lifted-code self)
     (lifted-variable-var self))]

  ;; #:methods gen:inferable
  #; [(define (infer-type self)
     (lifted-variable-type self))])
     
(struct lifted-apply (proc args) #:transparent
  #:methods gen:lifted
  [(define/generic gen-eval-lifted eval-lifted)
   (define/generic gen-lifted-code lifted-code)
   
   (define (eval-lifted self)
     (apply (gen-eval-lifted (lifted-apply-proc self))
            (map gen-eval-lifted (lifted-apply-args self))))

   (define (lifted-code self)
     `(,(gen-lifted-code (lifted-apply-proc self))
       ,@(map gen-lifted-code (lifted-apply-args self))))]

  ;; #:methods gen:inferable
  #; [(define (infer-type self)
     ;; TODO
     3)])
     
(struct lifted-begin (args) #:transparent
  #:methods gen:lifted
  [(define/generic gen-eval-lifted eval-lifted)
   (define/generic gen-lifted-code lifted-code)
   
   (define (eval-lifted self)
     (for ([arg (lifted-begin-args self)])
       (gen-eval-lifted arg)))

   (define (lifted-code self)
     `(begin ,@(map gen-lifted-code (lifted-begin-args self))))]

  #:methods gen:inferable
  [(define (infer-type self)
     (infer-type (last (lifted-begin-args self))))])

(struct lifted-error () #:transparent
  #:methods gen:lifted
  [(define (eval-lifted self)
     (error "Default error -- LIFTED-ERROR"))

   (define (lifted-code self)
     `(error "Default error -- LIFTED-ERROR"))]

  #:methods gen:inferable
  [(define (infer-type self)
     ;; TODO: Implement
     3)])

(define (lift value var-name)
  (cond [(and (procedure? value) (symbol? var-name))
         (lambda arguments
           (lifted-apply (lifted-variable value var-name) arguments))]
        [(symbol? var-name)
         (lifted-variable value var-name)]
        [else
         (error (format "Cannot lift ~a which has value ~a~%"
                        var-name value))]))

(define (begin^ . args)
  (lifted-begin args))

(define-syntax (define-lifted stx)
  (syntax-case stx ()
    [(define-lifted [thing new-name] ...)
     (syntax/loc stx
       (begin (define new-name (lift thing 'thing))
              ...))]))
