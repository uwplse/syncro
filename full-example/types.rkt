#lang racket

(provide Integer-type Enum-type Vector-type
         Type% Integer% Enum% Vector%)

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

    (define/public (is-supertype? type)
      (if (class? type)
          (is-a? this type)
          (send type is-subtype? this)))

    (define/public (is-subtype? type)
      (is-a? type Type%))

    (define/public (repr)
      '(new Type%))

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

    (define/override (is-subtype? type)
      (is-a? type Integer%))

    (define/override (repr)
      '(new Integer%))

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
                     (list old-val-tmp)
                     (list this))]
            
            [else (super old-values-code update-type var)]))

    (define/override (symbolic-update-code update-type var)
      (cond [(equal? update-type 'assign)
             (define val-tmp (gensym 'val))
             (values #`(define-symbolic* #,val-tmp integer?)
                     #`(set! #,var #,val-tmp)
                     (list val-tmp)
                     (list this)
                     (list val-tmp))]

            [(equal? update-type 'increment)
             (values #'(void)
                     #`(set! #,var (+ #,var 1))
                     (list)
                     (list)
                     (list))]

            [(equal? update-type 'decrement)
             (values #'(void)
                     #`(set! #,var (- #,var 1))
                     (list)
                     (list)
                     (list))]

            [else (super symbolic-update-code update-type var)]))))

(define (Integer-type) (new Integer%))

;; TODO: Currently Enums can be used in the place of Integers. This
;; doesn't really fit in with Enums -- it might be better to have
;; separate Enum and Bounded-Integer types.
(define Enum%
  (class Integer%
    (super-new)
    (init-field num-items)
    (define/public (get-num-items) num-items)

    ;; TODO: Different Enums should not be able to replace each
    ;; other. Should we compare by identity? There are programmability
    ;; issues -- what if the programmer keeps saying (Enum-type 10)
    ;; instead of creating a single type and using it consistently.
    ;; Maybe we could make Enum-type expand to a define, so as to
    ;; force the programmer to give the type a name.
    ;; Until then, Enums will be allowed to replace each other.
    (define/override (is-subtype? type)
      (and (is-a? type Enum%)
           (< (send type get-num-items) num-items)))

    (define/override (repr)
      `(new Enum% [num-items ,num-items]))

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
                     (list old-val-tmp)
                     (list this))]

            [else (super old-values-code update-type var)]))

    (define/override (symbolic-update-code update-type var)
      (cond [(equal? update-type 'assign)
             (define val-tmp (gensym 'val))
             (values #`(begin (define-symbolic* #,val-tmp integer?)
                              (assert (>= #,val-tmp 0))
                              (assert (< #,val-tmp #,num-items)))
                     #`(set! #,var #,val-tmp)
                     (list val-tmp)
                     (list this)
                     (list val-tmp))]

            [else (super symbolic-update-code update-type var)]))))

(define (Enum-type items) (new Enum% [num-items items]))

;; TODO: Maybe allow Vectors to use an Enum type as the input-type,
;; instead of just storing a length? For example, if we specify that
;; the input type for word->topic is Words (an Enum type), then we can
;; make sure to only use Words as indices into it during synthesis.
(define Vector%
  (class Type%
    (super-new)
    (init-field len output-type)

    (define/public (get-len) len)
    (define/public (get-output-type) output-type)

    (define/override (is-subtype? other-type)
      (and (is-a? other-type Vector%)
           (< len (send other-type get-len))
           (send output-type is-subtype?
                 (send other-type get-output-type))))

    (define/override (repr)
      `(new Vector% [len ,len] [output-type ,(send output-type repr)]))

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
                    #`(define #,old-val-tmp (vector-ref #,var #,(car update-args)))
                    (list old-val-tmp)
                    (list output-type))))]
            
            [else (super old-values-code update-type var)]))

    (define/override (symbolic-update-code update-type var)
      (cond [(equal? update-type 'assign)
             (define tmp-index (gensym 'index))
             (define tmp-val (gensym 'val))

             (if (send output-type mutable-structure?)

                 (let-values ([(output-defns output-update output-update-symbolic-vars output-update-symbolic-vars-types output-update-args)
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
                    ;; TODO: When Vectors get an input type, replace the Enum-type below with input type
                    (cons (Enum-type len) output-update-symbolic-vars-types)
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
                    ;; TODO: When Vectors get an input type, replace the Enum-type below with input type
                    (list (Enum-type len) output-type)
                    (list tmp-index tmp-val))))]

            [else (super symbolic-update-code update-type var)]))))

(define (Vector-type length-or-input output)
  (define length
    (if (is-a? length-or-input Enum%)
        (send length-or-input get-num-items)
        length-or-input))
  (new Vector% [len length] [output-type output]))

