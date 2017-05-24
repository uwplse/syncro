#lang racket

(require (for-syntax syntax/parse)
         ;; Program parsing and synthesis framework
         "cmd-parse.rkt"
         "dependency-graph.rkt"
         "program.rkt"
         "synthesis.rkt"
         "../rosette/variable.rkt"

         ;; New data structures, operators, and types for the user
         "../rosette/enum-set.rkt"
         "../rosette/graph.rkt"
         "../rosette/operators.rkt"
         "../rosette/record.rkt"
         "../rosette/types.rkt")

(provide (all-from-out "../rosette/enum-set.rkt") ;; TODO: Remove the "enum-"
         (all-from-out "../rosette/graph.rkt")
         (all-from-out "../rosette/operators.rkt")
         (all-from-out "../rosette/record.rkt")
         (except-out (all-from-out "../rosette/types.rkt") Enum-type)
         incremental my-for/sum my-for/or my-for/and)

;;;;;;;;;;;;;;;;;;;;;
;; Overall Program ;;
;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (incremental expr ...)
  (begin
    ;; TODO: Currently we have to thread prog by passing it to all of the
    ;; helper macros. Is there a better way to do this?
    (define prog (make-program))
    (desugar prog expr) ...
    (finalize-program prog)
    (for-each
     pretty-print
     (perform-synthesis prog (cmd-parse (current-command-line-arguments))))))

(define (finalize-program p)
  ;; Constants are added in reverse order, so fix them here
  (set-program-constants! p (reverse (program-constants p))))

;; Desugars top-level expressions in the incremental form. Primarily
;; threads the prog variable through the other helper macros.
(define-syntax (desugar stx)
  (syntax-case stx (define define-enum-type define-structure algorithm)
    [(_ prog (define expr ...))
     (syntax/loc stx (define-constant-prog prog expr ...))]
    [(_ prog (define-enum-type expr ...))
     (syntax/loc stx (define-enum-type-prog prog expr ...))]
    [(_ prog (define-structure expr ...))
     (syntax/loc stx (define-structure-prog prog expr ...))]
    [(_ prog (algorithm expr ...))
     (syntax/loc stx (set-program-algorithm! prog '(begin expr ...)))]
    [(_ prog expr)
     (printf "WARNING: Unknown expression: ~a~%" (syntax->datum #'expr))
     (syntax/loc stx expr)]))

;;;;;;;;;;;;;;;;
;; Constructs ;;
;;;;;;;;;;;;;;;;

(define (add-constant! prog c)
  (set-program-constants! prog (cons c (program-constants prog))))

;; Defines a constant in Racket and stores metadata in prog.
(define-syntax (define-constant-prog stx)
  (syntax-parse stx
    #:context 'define
    [(_ prog var
        (~or (~optional (~seq #:type type-exp:expr)
                        #:name "#:type option")
             ;; ~and forces this to match both #:for-types and for-types?
             ;; This causes it to both check that the term is the
             ;; keyword #:for-types, and also binds the attribute
             ;; for-types? to #:for-types, which we can later use to
             ;; test whether we saw #:for-types or not.
             (~optional (~and #:for-types for-types?)
                        #:name "#:for-types flag")
             ;; You can either specify configs (which must be a list
             ;; of numbers) or a value (or neither), but not both.
             (~optional (~or (~seq #:configs [config-val:number ...])
                             (~seq #:value value-expr:expr))
                        #:name "#:configs or #:value option")) ...)
     ;; When defining this variable in the Rosette program, we either
     ;; need an implementation, or a type (on which we can call
     ;; make-symbolic to get a value)
     #:fail-unless (or (attribute type-exp)
                       (attribute value-expr)
                       (attribute config-val))
     "Requires at least one of #:type, #:value and #:configs"
     ;; #:for-types means that we must define the variable in Racket,
     ;; which means that we need an implementation (symbolic won't
     ;; work).
     #:fail-when (and (attribute for-types?)
                      (not (or (attribute value-expr)
                               (attribute config-val))))
     "Need #:value or #:configs when using #:for-types"

     ;; attribute returns the value of the attribute if it was
     ;; matched, and #f otherwise
     (let* ([has-type? (attribute type-exp)]
            [for-types? (attribute for-types?)]
            [for-types?-stx (if for-types? #'#t #'#f)]
            [has-value? (attribute value-expr)]
            [has-config? (attribute config-val)]
            [t-stx (if has-type? #'(#:type type-exp) #'())]
            [e-stx (if has-value? #'(#:expression 'value-expr) #'())]
            [c-stx (if has-config? #'(#:configs '(config-val ...)) #'())]
            [val-stx (cond [has-value? #'value-expr]
                           [has-config? #'(make-configurable 'var)]
                           [else #'#f])]
            [def-stx (if for-types? #`((define var #,val-stx)) #'())])
       (quasisyntax/loc stx
         (begin
           #,@def-stx
           (let ([tmp (make-constant 'var #,@t-stx #,@e-stx #,@c-stx
                                     #:for-types? #,for-types?-stx)])
             (add-constant! prog tmp)))))]))

;; Different Enums should not be able to replace each other. So, in
;; the subtype method we compare Enums by identity. However if the
;; programmer keeps creating new types by saying (Enum-type 10) over
;; and over instead of creating a single type and using it
;; consistently, then we would get weird behavior.
;; To prevent this, Enum-type expands to a define, so as to force the
;; programmer to give the type a name. This makes it obvious that they
;; are supposed to reuse that name in the program.
(define-syntax-rule (define-enum-type-prog prog name num)
  (define-constant-prog prog name #:value (Enum-type 'name num) #:for-types))

;; Any structure that should be added to the dependency graph, that
;; is, it will be the input/output in some synthesis problem.
(define-syntax (define-structure-prog stx)
  (define-syntax-class delta
    #:literals (define)
    (pattern (define (name:id [arg:id arg-type:expr] ...) body:expr ...)))

  (define-syntax-class sketch
    #:literals (lambda)
    (pattern (name (lambda (arg:id ...) body:expr ...))))

  (syntax-parse stx
    #:context 'define
    [(_ prog var
        (~or (~once (~seq #:type type-exp:expr)
                    #:name "#:type option")
             (~optional (~seq #:invariant inv:expr)
                        #:name "#:invariant option")
             ;; If we start generating code where we actually run the
             ;; algorithm, this should change from ~optional to ~once
             ;; to guarantee that every structure has an initial
             ;; value.
             (~optional (~or (~seq #:initialize initial:expr)
                             (~seq #:value val:expr))
                        #:name "#:initialize or #:value option")
             (~optional (~seq #:depends [par:id ...])
                        #:name "#:depends option")
             (~optional (~seq #:deltas [dlt:delta ...])
                        #:name "#:deltas option")
             (~optional (~seq #:sketches [skch:sketch ...])
                        #:name "#:sketches option")) ...)

     ;; Error checking.
     ;; Sketches are meant to help synthesis, but you only have
     ;; synthesis if you have a parent that can change
     ;; TODO: What if we specify #:sketches [], it shouldn't fail then
     #:fail-when (and (attribute skch) (not (attribute par)))
     "Can only add a sketch (#:sketches) if there is a dependency (#:depends)"
     #:fail-when (check-duplicate-identifier (or (attribute dlt.name) '()))
     "Cannot have duplicate deltas!"

     ;; Provide default values for all of the attributes.
     (with-syntax ([invariant-exp          (or (attribute inv)          #'#t)]
                   [initialize-exp         (or (attribute initial)      #'#f)]
                   [value-exp              (or (attribute val)          #'#f)]
                   [(parent ...)           (or (attribute par)          '())]
                   [(d.name ...)           (or (attribute dlt.name)     '())]
                   [((d.arg ...) ...)      (or (attribute dlt.arg)      '())]
                   [((d.arg-type ...) ...) (or (attribute dlt.arg-type) '())]
                   [((d.body ...) ...)     (or (attribute dlt.body)     '())]
                   [(s.name ...)           (or (attribute skch.name)    '())]
                   [((s.arg ...) ...)      (or (attribute skch.arg)     '())]
                   [((s.body ...) ...)     (or (attribute skch.body)    '())])
       (syntax/loc stx
         (begin
           ;; Create the node
           (define node
             (new node% [id 'var] [type type-exp] [invariant 'invariant-exp]
                  [init-code 'init-exp] [fn-code 'value-exp]
                  [delta-name->info
                   (make-hash
                    (list
                     (cons 'd.name
                           (make-delta-info '(d.arg ...)
                                            (list d.arg-type ...)
                                            '(d.body ...))) ...))]))

           ;; Add any sketches to the node
           (let ([ancestor (get-node-for-delta 's.name)])
             (send ancestor assert-delta-arg-names! 's.name
                   '(s.arg ...))
             (send node set-sketch! 's.name '(begin s.body ...)))
           ...

         ;; Put the node in the graph
         (add-node! (program-dependency-graph prog) 'var node)
         (for ([p '(parent ...)])
           (add-dependency! (program-dependency-graph prog) p 'var)))))]))

;;;;;;;;;;;;;;;;;;;
;; Miscellaneous ;;
;;;;;;;;;;;;;;;;;;;

;; NOTE: The reimplementations of for can also be found in
;; src/rosette/util.rkt
(define-syntax (my-for/sum stx)
  (syntax-case stx ()
    [(_ ([i itr] ...) expr ...)
     (syntax/loc stx
       (for/sum ([i itr] ...) expr ...))]))

;; NOTE: See caveat in src/rosette/util.rkt
(define-syntax (my-for/or stx)
  (syntax-case stx ()
    [(_ ([i itr] ...) expr ...)
     (syntax/loc stx
       (for/or ([i itr] ...) expr ...))]))

(define-syntax (my-for/and stx)
  (syntax-case stx ()
    [(_ ([i itr] ...) expr ...)
     (syntax/loc stx
       (for/and ([i itr] ...) expr ...))]))
