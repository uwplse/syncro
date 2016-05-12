#lang racket

(provide Integer-type Enum-type Vector-type
         define-incremental finalize)

;; TODO: Currently Type% is performing the role of an interface. If it
;; stays like that, we should convert it into an explicit interface.
(define Type%
  (class object%
    (super-new)

    (define/public (mutable-structure?)
      (error (format "~a does not implement mutable-structure?" this)))

    (define/public (symbolic-code var)
      (error (format "~a does not implement symbolic-code" this)))

    (define/public (update-code update-type)
      (error (format "~a does not implement update-code" this)))))

(define Integer%
  (class Type%
    (super-new)

    (define/override (mutable-structure?) #f)

    (define/override (symbolic-code var)
      #`(define-symbolic* #,var integer?))

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

            [else (super update-code update-type)]))))

(define (Integer-type) (new Integer%))

(define Enum%
  (class Type%
    (super-new)
    (init-field num-items)
    (define/public (get-num-items) num-items)

    (define/override (symbolic-code var)
      #`(begin (define-symbolic* #,var integer?)
               (assert (>= #,var 0))
               (assert (< #,var #,num-items))))

    (define/override (mutable-structure?) #f)

    (define/override (update-code update-type)
      (cond [(equal? update-type 'assign)
             (lambda (var val)
               #`(set! #,var #,val))]

            [else (super update-code update-type)]))))

(define (Enum-type items) (new Enum% [num-items items]))

(define Vector%
  (class Type%
    (super-new)
    (init-field len output-type)
    
    (define/override (symbolic-code var)
      (define tmp (gensym))
      #`(define #,var
          (build-vector #,len
                        (lambda (i)
                          #,(send output-type symbolic-code tmp)
                          #,tmp))))

    (define/override (mutable-structure?) #t)

    (define/override (update-code update-type)
      (cond [(equal? update-type 'assign)
             (if (send output-type mutable-structure?)

                 (lambda (vect index . args)
                   (apply (send output-type update-code update-type)
                          #`(vector-ref #,vect #,index)
                          args))

                 (lambda (vect index value)
                   #`(vector-set! #,vect #,index #,value)))]

            [else (super update-code update-type)]))))

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

    ;; TODO:
    ;; (define/public (get-symbolic-update-code)
    ;;   (send type symbolic-update-code))
    
    (define/public (get-base-update-code update-type . args)
      (apply (send type update-code update-type) args))))

(define id-table (make-hash))


(define-for-syntax (make-id template . ids)
  (string->symbol (apply format template (map syntax->datum ids))))

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
                 ((send (hash-ref id-table dep) get-update-fn 'recompute))))
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

(define (finalize)
  ;; Here we modify *values* in a hash table while iterating over that
  ;; hash table, so there is no problem.
  ;; Creates the children relation from the parents relation
  (for* ([(id struc) id-table]
         [parent (send struc get-parents)])
    (send (hash-ref id-table parent) add-child id)))
