#lang racket

(require "rosette-namespace.rkt")

(provide Integer-type Enum-type Vector-type
         define-constant define-incremental finalize
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

    (define/public (symbolic-update-code update-type var [varset-name #f])
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

    (define/override (symbolic-update-code update-type var [varset-name #f])
      (cond [(equal? update-type 'assign)
             (define tmp (gensym 'val))
             (values #`(begin (define-symbolic* #,tmp integer?)
                              #,(set-add-code varset-name tmp))
                     #`(set! #,var #,tmp))]

            [(equal? update-type 'increment)
             (values #'(void)
                     #`(set! #,var (+ #,var 1)))]

            [(equal? update-type 'decrement)
             (values #'(void)
                     #`(set! #,var (- #,var 1)))]

            [else (super symbolic-update-code update-type var varset-name)]))))

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

    (define/override (symbolic-update-code update-type var [varset-name #f])
      (cond [(equal? update-type 'assign)
             (define tmp (gensym 'val))
             (values #`(begin (define-symbolic* #,tmp integer?)
                              #,(set-add-code varset-name tmp)
                              (assert (>= #,tmp 0))
                              (assert (< #,tmp #,num-items)))
                     #`(set! #,var #,tmp))]

            [else (super symbolic-update-code update-type var varset-name)]))))

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

    (define/override (symbolic-update-code update-type var [varset-name #f])
      (cond [(equal? update-type 'assign)
             (define tmp-index (gensym 'index))
             (define tmp-val (gensym 'val))

             (if (send output-type mutable-structure?)

                 (let-values ([(output-defns output-update)
                               (send output-type symbolic-update-code
                                     ;; TODO: update-type needs to change here
                                     update-type
                                     #`(vector-ref #,var #,tmp-index)
                                     varset-name)])
                   (values
                    ;; Create the symbolic index into the vector
                    #`(begin (define-symbolic* #,tmp-index integer?)
                             #,(set-add-code varset-name tmp-index)
                             (assert (>= #,tmp-index 0))
                             (assert (< #,tmp-index #,len))
                             #,output-defns)
                    ;; Update the mutable structure at the specified symbolic index
                    output-update))

                 (values
                  ;; Create the symbolic index into the vector
                  #`(begin (define-symbolic* #,tmp-index integer?)
                           #,(set-add-code varset-name tmp-index)
                           (assert (>= #,tmp-index 0))
                           (assert (< #,tmp-index #,len))
                           ;; Create the symbolic value
                           #,(send output-type symbolic-code tmp-val varset-name))
                  ;; Perform the update
                  #`(vector-set! #,var #,tmp-index #,tmp-val)
                  tmp-index))]

            [else (super symbolic-update-code update-type var varset-name)]))))

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
      (hash-ref update-fns update-type))

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
    
    (define/public (get-symbolic-update-code update-type var [varset-name #f])
      (send type symbolic-update-code update-type var varset-name))
    
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

           (begin
             ;; Define the update functions for each update type
             (define (update-id . args)
               ;; TODO: Hard coded for testing
               (define old-value
                 (if (equal? 'name 'word->topic)
                     (vector-ref name (first args))
                     #f))
               ;; First apply the update to the data structure
               ;; TODO: eval only works if the free variables in the
               ;; code are globally defined, I think
               (eval
                (send/apply struc get-base-update-code 'update-type #'name args))
               ;; Then propagate it to its children in the dependency graph
               ;; TODO: This can recompute things multiple times. If we
               ;; assert that the dependency graph must be a DAG, we
               ;; could find a linearization and process updates in that
               ;; order.
               (for ([dep (send struc get-children)])
                 ;; TODO: Hard coded for testing
                 (if (equal? (send (hash-ref id-table dep) get-id) 'num1)
                     ;; TODO: Should actually be the following:
                     ;(apply (send (hash-ref id-table dep) get-update-fn 'name 'update-type) args)
                     ((send (hash-ref id-table dep) get-update-fn 'update-type) name old-value (second args))
                     ((send (hash-ref id-table dep) get-update-fn 'recompute)))))
             ...)

           ;; Add the update functions for each update type to struc
           (begin (send struc set-update-fn 'update-type update-id)
                  ...)
           ;; Add the recompute update function to struc
           (send struc set-update-fn 'recompute
                 (lambda ()
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
    
    (define-namespace-anchor anchor)
    (define program-ns (namespace-anchor->namespace anchor))
    
    (define parent (hash-ref id-table 'word->topic))
    (define child (hash-ref id-table 'num1))
    (let* ([p-id (send parent get-id)]
           [c-id (send child get-id)]
           [update-type 'assign]
           [sketch-name 'sketch]
           [parent-definition (send parent get-symbolic-code p-id 'symbolic-vars)]
           [child-expr (send child get-fn-code)])
      ;; TODO: index-var is a hack to test, get rid of it
      (let-values ([(update-defns-code update-code index-var)
                    (send parent get-symbolic-update-code update-type
                          p-id 'symbolic-vars)])
        (define rosette-code
          `(begin
             (define symbolic-vars (mutable-set))
             ;(define terminal-hash (make-hash))
             ,(syntax->datum parent-definition)
             (define ,c-id ,child-expr)
             ,(syntax->datum update-defns-code)
             (define synth
               (synthesize
                #:forall (set->list symbolic-vars)
                #:guarantee
                (begin
                  ;; NOTE: One possible way to perform this synthesis is
                  ;; to have one function run before the update and one
                  ;; run after the update. Alternatively, we could save
                  ;; the old values, and then synthesize a single function
                  ;; that can access those old values.
                  ;; It seems Rosette's define-synthax can only be used
                  ;; once per synthesis problem (current hypothesis: it
                  ;; returns the same function if used again, because all
                  ;; of the symbolic variables are the same, because the
                  ;; call stack is the same), so we're going with the
                  ;; latter option for now.
                  ;; TODO: Hard coded for testing
                  (define old-value (vector-ref ,p-id ,index-var))
                  ,(syntax->datum update-code)
                  ;; TODO: Hard coded for testing
                  (define new-value (vector-ref ,p-id ,index-var))
                  (,sketch-name ,c-id old-value new-value)
                  (assert (equal? ,c-id ,child-expr)))))
             (syntax->datum (car (generate-forms synth)))))
        ;(pretty-print rosette-code)
        (define result (eval rosette-code rosette-ns))
        (pretty-print result)
        (eval result program-ns)
        (define update-fn (eval sketch-name program-ns))
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
