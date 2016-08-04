#lang racket

(require "rosette-namespace.rkt" "types.rkt" "variables.rkt")

(provide (all-from-out "types.rkt")
         define-constant define-incremental finalize
         id-table
         my-for/sum my-for/or)

(define structure%
  (class* object% (writable<%>)
    (super-new)

    (init-field id type update-types children parents fn fn-code)
    (init-field [update-fns (make-hash)])

    (define/public (custom-write port)
      (display (format "#<structure%:~a>" id) port))

    (define/public (custom-display port)
      (display (format "#<structure%:~a>" id) port))

    (define/public (get-id) id)
    (define/public (get-type) type)
    (define/public (get-children) children)
    (define/public (get-parents) parents)
    (define/public (get-fn) fn)
    (define/public (get-fn-code) fn-code)
    (define/public (get-update-fn update-type)
      (if (and (not (equal? update-type 'recompute))
               (not (hash-has-key? update-fns update-type)))
          (begin (display (format "Warning: Using recomputation instead of update of type ~a to ~a~%" update-type id))
                 (hash-ref update-fns 'recompute))
          (hash-ref update-fns update-type)))

    (define/public (add-child child)
      (set! children (cons child children)))
    (define/public (add-parent parent)
      (set! parents (cons parent parents)))
    (define/public (set-fn val)
      (set! fn val))
    (define/public (set-fn-code code)
      (set! fn-code code))
    (define/public (set-update-fn update-type fn [may-exist? #f])
      (when (and (not may-exist?)
                 (hash-has-key? update-fns update-type))
        (error (format "Update function for ~a already exists!"
                       update-type)))
      (hash-set! update-fns update-type fn))

    (define/public (get-symbolic-code var [varset-name #f])
      (symbolic-code type var varset-name))
    
    (define/public (get-symbolic-update-code update-type var)
      (symbolic-update-code type update-type var))
    
    (define/public (get-old-values-code update-type var . update-args)
      (apply old-values-code type update-type var update-args))
    
    (define/public (get-base-update-code update-type . args)
      (apply (update-code type update-type) args))))

(define id-table (make-hash))

(define-for-syntax (make-id template . ids)
  (if (symbol? (car ids))
      (string->symbol (apply format template ids))
      (string->symbol (apply format template (map syntax->datum ids)))))

(define-syntax (define-incremental stx)
  (syntax-case stx ()
    [(_ name type-exp (dependency ...) (update-type ...) expr ...)
     (with-syntax ([(update-id ...)
                    (map (lambda (x)
                           (datum->syntax stx (make-id "~a-~a!" x #'name)))
                         (syntax-e #'(update-type ...)))])
       (syntax/loc stx
         ;; TODO: type-exp is recomputed twice here, but don't want to
         ;; use a let because then update-id would not be global
         (begin
           (when (hash-has-key? id-table 'name)
             (error (format "Symbol has already been used: ~a" 'name)))
           
           (define struc
             (new structure%
                  [id 'name] [type type-exp]
                  [update-types '(update-type ...)]
                  [children '()] [parents '(dependency ...)]
                  [fn (lambda () expr ...)]
                  [fn-code '(begin expr ...)]))

           (define-namespace-anchor program-anchor)
           (define program-ns (namespace-anchor->namespace program-anchor))

           (begin
             ;; Define the update functions for each update type
             (define (update-id . args)
               (let-values ([(update-old-vals update-old-val-vars _)
                             (send/apply struc get-old-values-code 'update-type #'name args)])
                 ;; Define the old values
                 (eval (syntax->datum update-old-vals) program-ns)

                 ;; Apply the update to the data structure
                 (eval
                  (syntax->datum
                   (send/apply struc get-base-update-code 'update-type #'name args))
                  program-ns)
                 ;; Then propagate it to its children in the dependency graph
                 ;; TODO: This can recompute things multiple times. If we
                 ;; assert that the dependency graph must be a DAG, we
                 ;; could find a linearization and process updates in that
                 ;; order. This may also be necessary for correctness...
                 (for ([dep (send struc get-children)])
                   ;; TODO: Hard coded for testing, should actually be the following:
                   ;(apply (send (hash-ref id-table dep) get-update-fn 'name 'update-type) args)
                   (apply (send (hash-ref id-table dep) get-update-fn 'update-type)
                          (append (map (lambda (var) (eval var program-ns))
                                       update-old-val-vars)
                                  args)))))
             ...)

           ;; Add the update functions for each update type to struc
           (begin (send struc set-update-fn 'update-type update-id)
                  ...)
           ;; Add the recompute update function to struc
           (send struc set-update-fn 'recompute
                 (lambda args
                   (set! name (begin expr ...))
                   (for ([dep (send struc get-children)])
                     ((send (hash-ref id-table dep) get-update-fn 'recompute)))))
           
           (hash-set! id-table 'name struc)

           (define name (begin expr ...))
           (void))))]))

;; TODO: Refactor so that variables (symbolic or not) are attached to their types
(define-syntax-rule (finalize)
  (begin
    ;; Implementation Detail: Here we modify *values* in a hash table
    ;; while iterating over that hash table, so there is no problem.
    ;; Creates the children relation from the parents relation
    (for* ([(id struc) id-table]
           [parent (send struc get-parents)])
      (send (hash-ref id-table parent) add-child id))

    (define-namespace-anchor program-anchor)
    (define program-ns (namespace-anchor->namespace program-anchor))
    ;; TODO: Document all variable names with an example program
    (define parent (hash-ref id-table 'word->topic))
    (for ([dummy '(dummy)]) ;c-id (send parent get-children)])
      (let* ([c-id 'num1]
             [p-id (send parent get-id)]
             [p-type (send parent get-type)]
             [child (hash-ref id-table c-id)]
             [c-type (send child get-type)]
             [update-type 'assign]
             [parent-definition (send parent get-symbolic-code p-id 'symbolic-defn-vars-set)]
             [child-definition (send child get-fn-code)]
             [constant-vars (reverse (constant-terminal-list))]
             [constant-types (map (lambda (key) (hash-ref constant-terminal-types key))
                                  constant-vars)]
             [constant-defns (reverse (constant-definitions))])
        (let*-values ([(update-defns-code update-code update-symbolic-vars update-symbolic-vars-types update-args)
                       ;; TODO: Add comments here
                       (send parent get-symbolic-update-code update-type p-id)]
                      [(overwritten-vals-definition overwritten-vals overwritten-vals-types)
                       (send/apply parent get-old-values-code update-type p-id update-args)])

          (define (add-terminal-code var type #:mutable [mutable #f])
            `(send terminal-info add-terminal ',var ,var ,(repr type) #:mutable ,mutable))
          
          (define rosette-code
            `(let ()
               ,@constant-defns
               (define symbolic-defn-vars-set (mutable-set))
               ,(syntax->datum parent-definition)
               (define ,c-id ,child-definition)
               ,(syntax->datum update-defns-code)
               ,(syntax->datum overwritten-vals-definition)
               (define symbolic-defn-vars (set->list symbolic-defn-vars-set))
               (define terminal-info (new Terminal-Info%))
               ,(add-terminal-code c-id c-type #:mutable #t)
               ,(add-terminal-code p-id p-type)
               ,@(map add-terminal-code overwritten-vals overwritten-vals-types)
               ,@(map add-terminal-code update-symbolic-vars update-symbolic-vars-types)
               ,@(map add-terminal-code constant-vars constant-types)
               (define program (grammar terminal-info 3 3))
               (define synth
                 (time
                  (synthesize
                   #:forall (append (list ,@update-symbolic-vars) symbolic-defn-vars)
                   #:guarantee
                   (begin
                     ;; NOTE: Another possible way to perform this synthesis is
                     ;; to have one function run before the update and one run
                     ;; after the update.
                     ,(syntax->datum update-code)
                     (eval-lifted program)
                     (assert (equal? ,c-id ,child-definition))))))
               (evaluate (lifted-code program) synth)))
          (pretty-print rosette-code)
          (define result (eval rosette-code rosette-ns))
          ;(pretty-print result)
          (define update-fn-code
            `(lambda ,(append overwritten-vals update-symbolic-vars)
               ,result
               (for ([dep (send (hash-ref id-table ',c-id) get-children)])
                 ;; TODO: Hard coded for testing, see previous example for how to change this
                 ((send (hash-ref id-table dep) get-update-fn 'recompute)))))
          (pretty-print update-fn-code)
          (define update-fn (eval update-fn-code program-ns))
          (send child set-update-fn update-type update-fn))))))

;; NOTE: The reimplementations of for can also be found in
;; rosette-namespace.rkt
(define-syntax (my-for/sum stx)
  (syntax-case stx ()
    [(_ ([i itr] ...) expr ...)
     (syntax/loc stx
       (let ([sum 0])
         (for ([i itr] ...)
           (set! sum (+ sum (begin expr ...))))
         sum))]))

;; TODO: Reimplement with break? If so, also change in
;; rosette-namespace.rkt
(define-syntax (my-for/or stx)
  (syntax-case stx ()
    [(_ ([i itr] ...) expr ...)
     (syntax/loc stx
       (let ([val #f])
         (for ([i itr] ...)
           (set! val (or val (begin expr ...))))
         val))]))
