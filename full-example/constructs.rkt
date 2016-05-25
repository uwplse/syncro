#lang racket

(require "rosette-namespace.rkt")

(provide Integer-type Enum-type Vector-type
         define-constant define-incremental finalize
         id-table
         my-for/sum my-for/or)

;; TODO: Also put in the define-symbolic into this function
(define (set-add-code set-name val)
  (if set-name
      #`(set-add! #,set-name #,val)
      #'(void)))

;; TODO: Currently Type% is performing the role of an interface. If it
;; stays like that, we should convert it into an explicit interface.
(define Type%
  (class object%
    (super-new)

    (define/public (mutable-structure?)
      (error (format "~a does not implement mutable-structure?" this)))

    (define/public (symbolic-code var [varset-name #f])
      (error (format "~a does not implement symbolic-code" this)))

    (define/public (update-code update-type)
      (error (format "~a does not implement update-code" this)))

    (define/public (old-values-code update-type var . update-args)
      (error (format "~a does not implement old-values-code" this)))

    (define/public (symbolic-update-code update-type var)
      (error (format "~a does not implement symbolic-update-code" this)))))

(define Integer%
  (class Type%
    (super-new)

    (define/override (mutable-structure?) #f)

    (define/override (symbolic-code var [varset-name #f])
      #`(begin (define-symbolic* #,var integer?)
               #,(set-add-code varset-name var)))

    (define/override (update-code update-type)
      (cond [(equal? update-type 'assign)
             (lambda (var val)
               #`(set! #,var #,val))]

            [(equal? update-type 'increment)
             (lambda (var)
               #`(set! #,var (+ #,var 1)))]

            [(equal? update-type 'decrement)
             (lambda (var)
               #`(set! #,var (- #,var 1)))]

            [else (super update-code update-type)]))

    (define/override (old-values-code update-type var . update-args)
      (define old-val-tmp (gensym 'old-value))
      (cond [(member update-type '(assign increment decrement))
             (values #`(define #,old-val-tmp #,var)
                     (list old-val-tmp))]
            
            [else (super old-values-code update-type var)]))

    (define/override (symbolic-update-code update-type var)
      (cond [(equal? update-type 'assign)
             (define val-tmp (gensym 'val))
             (values #`(define-symbolic* #,val-tmp integer?)
                     #`(set! #,var #,val-tmp)
                     (list val-tmp)
                     (list val-tmp))]

            [(equal? update-type 'increment)
             (values #'(void)
                     #`(set! #,var (+ #,var 1))
                     (list)
                     (list))]

            [(equal? update-type 'decrement)
             (values #'(void)
                     #`(set! #,var (- #,var 1))
                     (list)
                     (list))]

            [else (super symbolic-update-code update-type var)]))))

(define (Integer-type) (new Integer%))

(define Enum%
  (class Type%
    (super-new)
    (init-field num-items)
    (define/public (get-num-items) num-items)

    (define/override (mutable-structure?) #f)

    (define/override (symbolic-code var [varset-name #f])
      #`(begin (define-symbolic* #,var integer?)
               #,(set-add-code varset-name var)
               (assert (>= #,var 0))
               (assert (< #,var #,num-items))))

    (define/override (update-code update-type)
      (cond [(equal? update-type 'assign)
             (lambda (var val)
               #`(set! #,var #,val))]

            [else (super update-code update-type)]))

    (define/override (old-values-code update-type var . update-args)
      (cond [(equal? update-type 'assign)
             (define old-val-tmp (gensym 'old-value))
             (values #`(define #,old-val-tmp #,var)
                     (list old-val-tmp))]

            [else (super old-values-code update-type var)]))

    (define/override (symbolic-update-code update-type var)
      (cond [(equal? update-type 'assign)
             (define val-tmp (gensym 'val))
             (values #`(begin (define-symbolic* #,val-tmp integer?)
                              (assert (>= #,val-tmp 0))
                              (assert (< #,val-tmp #,num-items)))
                     #`(set! #,var #,val-tmp)
                     (list val-tmp)
                     (list val-tmp))]

            [else (super symbolic-update-code update-type var)]))))

(define (Enum-type items) (new Enum% [num-items items]))

(define Vector%
  (class Type%
    (super-new)
    (init-field len output-type)

    (define/override (mutable-structure?) #t)
    
    (define/override (symbolic-code var [varset-name #f])
      (define tmp (gensym))
      #`(define #,var
          (build-vector #,len
                        (lambda (i)
                          #,(send output-type symbolic-code tmp varset-name)
                          #,tmp))))

    (define/override (update-code update-type)
      (cond [(equal? update-type 'assign)
             (if (send output-type mutable-structure?)

                 (lambda (vect index . args)
                   (apply (send output-type update-code update-type)
                          #`(vector-ref #,vect #,index)
                          args))

                 (lambda (vect index value)
                   #`(vector-set! #,vect #,index #,value)))]

            [else (super update-code update-type)]))

    (define/override (old-values-code update-type var . update-args)
      (cond [(equal? update-type 'assign)
             (if (send output-type mutable-structure?)
                 (send/apply output-type old-values-code
                             ;; TODO: update-type needs to change here
                             update-type
                             #`(vector-ref #,var #,(car update-args))
                             (cdr update-args))

                 (let ([old-val-tmp (gensym 'old-value)])
                   (values
                    ;; TODO: tmp-index no longer exists...
                    #`(define #,old-val-tmp (vector-ref #,var #,(car update-args)))
                    (list old-val-tmp))))]

            [else (super old-values-code update-type var)]))

    (define/override (symbolic-update-code update-type var)
      (cond [(equal? update-type 'assign)
             (define tmp-index (gensym 'index))
             (define tmp-val (gensym 'val))

             (if (send output-type mutable-structure?)

                 (let-values ([(output-defns output-update output-update-symbolic-vars output-update-args)
                               (send output-type symbolic-update-code
                                     ;; TODO: update-type needs to change here
                                     update-type
                                     #`(vector-ref #,var #,tmp-index))])
                   (values
                    ;; Create the symbolic index into the vector
                    #`(begin (define-symbolic* #,tmp-index integer?)
                             (assert (>= #,tmp-index 0))
                             (assert (< #,tmp-index #,len))
                             #,output-defns)
                    ;; Update the mutable structure at the specified symbolic index
                    output-update
                    (cons tmp-index output-update-symbolic-vars)
                    (cons tmp-index output-update-args)))

                 (let ([old-val-tmp (gensym 'old-value)])
                   (values
                    ;; Create the symbolic index into the vector
                    #`(begin (define-symbolic* #,tmp-index integer?)
                             (assert (>= #,tmp-index 0))
                             (assert (< #,tmp-index #,len))
                             ;; Create the symbolic value
                             ;; TODO: This isn't an issue now, since output-type cannot be a mutable structure, but what if symbolic-code creates other symbolic variables besides tmp-val?
                             #,(send output-type symbolic-code tmp-val #f))
                    ;; Perform the update
                    #`(vector-set! #,var #,tmp-index #,tmp-val)
                    ;; TODO: Get the other symbolic variables here
                    (list tmp-index tmp-val)
                    (list tmp-index tmp-val))))]

            [else (super symbolic-update-code update-type var)]))))

(define (Vector-type length-or-input output)
  (define length
    (if (is-a? length-or-input Enum%)
        (send length-or-input get-num-items)
        length-or-input))
  (new Vector% [len length] [output-type output]))

(define structure%
  (class object%
    (super-new)

    (init-field id type update-types children parents fn fn-code)
    (init-field [update-fns (make-hash)])

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
      (send type symbolic-code var varset-name))
    
    (define/public (get-symbolic-update-code update-type var)
      (send type symbolic-update-code update-type var))
    
    (define/public (get-old-values-code update-type var . update-args)
      (send/apply type old-values-code update-type var update-args))
    
    (define/public (get-base-update-code update-type . args)
      (apply (send type update-code update-type) args))))

(define id-table (make-hash))


(define-for-syntax (make-id template . ids)
  (string->symbol (apply format template (map syntax->datum ids))))

(define-syntax-rule (define-constant var val)
  (begin (define var val)
         (eval '(define var val) rosette-ns)))

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
               (let-values ([(update-old-vals update-old-val-vars)
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
                 ;; order.
                 (for ([dep (send struc get-children)])
                   ;; TODO: Hard coded for testing, should actually be the following:
                   ;(apply (send (hash-ref id-table dep) get-update-fn 'name 'update-type) args)
                   (apply (send (hash-ref id-table dep) get-update-fn 'update-type)
                          ;TODO: Include name, but first need to prevent the synthesized function from modifying it (see TODO in the synthesis part)
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

(define-syntax-rule (finalize)
  (begin
    ;; Here we modify *values* in a hash table while iterating over that
    ;; hash table, so there is no problem.
    ;; Creates the children relation from the parents relation
    (for* ([(id struc) id-table]
           [parent (send struc get-parents)])
      (send (hash-ref id-table parent) add-child id))

    (define-namespace-anchor program-anchor)
    (define program-ns (namespace-anchor->namespace program-anchor))
        
    (define parent (hash-ref id-table 'word->topic))
    (define child (hash-ref id-table 'num1))
    (let* ([p-id (send parent get-id)]
           [c-id (send child get-id)]
           [update-type 'assign]
           [sketch-name 'sketch]
           [parent-definition (send parent get-symbolic-code p-id 'symbolic-defn-vars-set)]
           [child-expr (send child get-fn-code)])
      (let*-values ([(update-defns-code update-code update-symbolic-vars update-args)
                     (send parent get-symbolic-update-code update-type p-id)]
                    [(update-old-vals update-old-val-vars)
                     (send/apply parent get-old-values-code update-type p-id update-args)])
        (define rosette-code
          `(begin
             (define symbolic-defn-vars-set (mutable-set))
             ;(define terminal-hash (make-hash))
             ,(syntax->datum parent-definition)
             (define ,c-id ,child-expr)
             ,(syntax->datum update-defns-code)
             (define symbolic-defn-vars (set->list symbolic-defn-vars-set))
             (define synth
               (synthesize
                #:forall (append (list ,@update-symbolic-vars) symbolic-defn-vars)
                #:guarantee
                (begin
                  ;; NOTE: Another possible way to perform this synthesis is
                  ;; to have one function run before the update and one run
                  ;; after the update.
                  ,(syntax->datum update-old-vals)
                  ,(syntax->datum update-code)
                  (,sketch-name ,c-id
                                ; TODO: Include ,p-id but prevent the sketch from mutating it, it should be read-only
                                ,@update-old-val-vars
                                ,@update-symbolic-vars)
                  (assert (equal? ,c-id ,child-expr)))))
             (syntax->datum (car (generate-forms synth)))))
        ;(pretty-print rosette-code)
        (define result (eval rosette-code rosette-ns))
        ;(pretty-print result)
        (define update-fn-code
          `(lambda args
             ,result
             (apply ,sketch-name ,c-id args)
             (for ([dep (send (hash-ref id-table ',c-id) get-children)])
               ;; TODO: Hard coded for testing, see previous example for how to change this
               ((send (hash-ref id-table dep) get-update-fn 'recompute)))))
        (pretty-print update-fn-code)
        (define update-fn (eval update-fn-code program-ns))
        (send child set-update-fn update-type update-fn)))))

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
