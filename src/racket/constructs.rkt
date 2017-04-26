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
         incremental algorithm my-for/sum my-for/or my-for/and)

(define-syntax-rule (incremental expr ...)
  (begin
    ;; TODO: Currently we have to thread prog by passing it to all of the
    ;; helper macros. Is there a better way to do this?
    (define prog (program '() '() (make-dependency-graph) '()))
    (desugar prog expr) ...
    (set-program-initialization! prog (reverse (program-initialization prog)))
    (set-program-constants! prog (reverse (program-constants prog)))
    (pretty-print
     (perform-synthesis prog (cmd-parse (current-command-line-arguments))))))

;; Desugars top-level expressions in the incremental form. Primarily
;; threads the prog variable through the other helper macros.
(define-syntax (desugar stx)
  (syntax-case stx (define define-enum-type define-incremental algorithm)
    [(_ prog (define expr ...))
     (syntax/loc stx (define-constant-prog prog expr ...))]
    [(_ prog (define-enum-type expr ...))
     (syntax/loc stx (define-enum-type-prog prog expr ...))]
    [(_ prog (define-incremental expr ...))
     (syntax/loc stx (define-incremental-prog prog expr ...))]
    [(_ prog (algorithm expr ...))
     (syntax/loc stx (set-program-algorithm! prog '(begin expr ...)))]
    [(_ prog expr)
     (syntax/loc stx expr)]))

;; Defines a constant in Racket and stores metadata in prog.
(define-syntax (define-constant-prog stx)
  (syntax-case stx ()
    [(_ prog var val)
     (syntax/loc stx
       (define-constant-prog prog var #f val))]
    [(_ prog var type-exp val)
     (syntax/loc stx
       (begin
         (set-program-initialization!
          prog
          (cons '(define var val) (program-initialization prog)))
         (set-program-constants!
          prog
          (cons (make-variable 'var #:type type-exp
                               #:definition '(define var val))
                (program-constants prog)))
         (define var val)
         (void)))]))

;; Different Enums should not be able to replace each other. So, in
;; the subtype method we compare Enums by identity. However if the
;; programmer keeps creating new types by saying (Enum-type 10) over
;; and over instead of creating a single type and using it
;; consistently, then we would get weird behavior.
;; To prevent this, Enum-type expands to a define, so as to force the
;; programmer to give the type a name. This makes it obvious that they
;; are supposed to reuse that name in the program.
(define-syntax-rule (define-enum-type-prog prog name items)
  (define-constant-prog prog name (Enum-type 'name items)))

;; Adds metadata to the dependency graph, and defines the data
;; structure in Racket.
(define-syntax (define-incremental-prog stx)
  (define-splicing-syntax-class maybe-assumes
    (pattern (~seq #:assume assumes:expr))
    (pattern (~seq) #:with assumes #'(list)))

  (define-splicing-syntax-class init-or-value
    (pattern (~seq #:initialize init:expr) #:with value #'#f)
    (pattern (~seq #:value value:expr) #:with init #'#f))
  
  (define-splicing-syntax-class maybe-depends
    ;; TODO: Better error messages -- check that parents is a list of ids.
    (pattern (~seq #:depends parents:expr))
    (pattern (~seq) #:with parents #'()))

  (define-splicing-syntax-class maybe-updates
    ;; TODO: Better error message (see above)
    (pattern (~seq #:updates updates:expr))
    (pattern (~seq) #:with updates #'()))

  (define-splicing-syntax-class maybe-sketches
    ;; TODO: Better error messages (see above)
    (pattern (~seq #:sketches sketches:expr))
    (pattern (~seq) #:with sketches #'()))

  (syntax-parse stx
    #:context 'define-incremental
    [(_ prog name type-exp a:maybe-assumes iv:init-or-value d:maybe-depends
        u:maybe-updates s:maybe-sketches)
     (syntax/loc stx
       (define-incremental-base prog name type-exp
         [assumes a.assumes]
         [initialize iv.init]
         [value iv.value]
         [depends d.parents]
         [updates u.updates]
         [sketches s.sketches]))]))

(define-syntax (define-incremental-base stx)
  (syntax-case stx (assumes initialize depends updates sketches lambda define)
    [(_ prog name type-exp [assumes assumption-expr]
        [initialize init-exp] [value val-exp] [depends [parent ...]]
        [updates [(define (update-name [update-arg update-arg-type] ...)
                    update-body ...) ...]]
        [sketches [(sketch-name (lambda (sketch-arg ...) sketch-expr ...))
                   ...]])
     (syntax/loc stx
       (begin
         ;; Check the validity of the input
         (unless (= (length '(update-name ...))
                    (length (remove-duplicates '(update-name ...))))
           (error "Cannot have duplicate updates!"))

         ;; Create the node
         (define node
           (new node% [id 'name] [type type-exp] [assumes 'assumption-expr]
                [init-code 'init-exp] [fn-code 'val-exp]
                [update-name->info
                 (make-hash
                  (list
                   (cons 'update-name
                         (make-update-info '(update-arg ...)
                                           (list update-arg-type ...)
                                           '(update-body ...))) ...))]))

         ;; Add any sketches to the node
         (let ([ancestor (get-node-for-update 'sketch-name)])
           (send ancestor assert-update-arg-names! 'sketch-name
                 '(sketch-arg ...))
           (send node set-sketch! 'sketch-name '(begin sketch-expr ...)))
         ...

         ;; Put the node in the graph
         (add-node! (program-dependency-graph prog) 'name node)
         (for ([p '(parent ...)])
           (add-dependency! (program-dependency-graph prog) p 'name))

         ;; Define the initial value
         (define name (or init-exp val-exp))
         (void)))]))

;; TODO: This is a placeholder
(define-syntax-rule (algorithm expr ...)
  (void))

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
