#lang rosette

(require (only-in "variables.rkt" define-untyped-constant))

(provide Integer-type define-enum-type Vector-type Type?
         Integr Enum Vectr Vectr-input-type Vectr-output-type
         is-subtype? repr mutable-structure? symbolic-code
         update-code old-values-code symbolic-update-code
         (rename-out [Type? Type%] [Integr? Integer%]
                     [Enum? Enum%] [Vectr? Vector%]))

;; TODO: Also put in the define-symbolic into this function
(define (set-add-code set-name val)
  (if set-name
      #`(set-add! #,set-name #,val)
      #'(void)))

(define-generics Type
  (is-subtype? Type other-type)
  (repr Type)
  (mutable-structure? Type)
  ;; TODO: Make varset-name optional
  (symbolic-code Type var varset-name)
  (update-code Type update-type)
  (old-values-code Type update-type var . update-args)
  (symbolic-update-code Type update-type var))

;; TODO: Fix naming here
(struct Integr ()
  #:methods gen:Type
  [(define (is-subtype? self other-type)
     (Integr? other-type))

   (define (repr self)
     '(Integr))

   (define (mutable-structure? self) #f)

   (define (symbolic-code self var varset-name)
     #`(begin (define-symbolic* #,var integer?)
              #,(set-add-code varset-name var)))

   (define (update-code self update-type)
     (cond [(equal? update-type 'assign)
            (lambda (var val)
              #`(set! #,var #,val))]

           [(equal? update-type 'increment)
            (lambda (var)
              #`(set! #,var (+ #,var 1)))]

           [(equal? update-type 'decrement)
            (lambda (var)
              #`(set! #,var (- #,var 1)))]

           [else
            (error (format "Unknown Integer update type: ~a~%"
                           update-type))]))

   (define (old-values-code self update-type var . update-args)
     (define old-val-tmp (gensym 'old-value))
     (cond [(member update-type '(assign increment decrement))
            (values #`(define #,old-val-tmp #,var)
                    (list old-val-tmp)
                    (list self))]

           [else
            (error (format "Unknown Integer update type: ~a~%"
                           update-type))]))

   (define (symbolic-update-code self update-type var)
     (cond [(equal? update-type 'assign)
            (define val-tmp (gensym 'val))
            (values #`(define-symbolic* #,val-tmp integer?)
                    #`(set! #,var #,val-tmp)
                    (list val-tmp)
                    (list self)
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

           [else
            (error (format "Unknown Integer update type: ~a~%"
                           update-type))]))])

(define (Integer-type) (Integr))

(struct Enum (name num-items) #:transparent
  #:methods gen:Type
  [;; If you have Word be (Enum% 3) and Topic be (Enum% 3), they are
   ;; still semantically different and Words should not replace
   ;; Topics. So, check subtyping by identity equality.
   (define (is-subtype? self other-type)
     (eq? self other-type))

   (define (repr self)
     (Enum-name self))

   (define (mutable-structure? self) #f)

   (define (symbolic-code self var varset-name)
     #`(begin (define-symbolic* #,var integer?)
              #,(set-add-code varset-name var)
              (assert (>= #,var 0))
              (assert (< #,var #,(Enum-num-items self)))))

   (define (update-code self update-type)
     (cond [(equal? update-type 'assign)
            (lambda (var val)
              #`(set! #,var #,val))]

           [else
            (error (format "Unknown Enum update type: ~a~%"
                           update-type))]))

   (define (old-values-code self update-type var . update-args)
     (cond [(equal? update-type 'assign)
            (define old-val-tmp (gensym 'old-value))
            (values #`(define #,old-val-tmp #,var)
                    (list old-val-tmp)
                    (list self))]

           [else
            (error (format "Unknown Enum update type: ~a~%"
                           update-type))]))

   (define (symbolic-update-code self update-type var)
     (cond [(equal? update-type 'assign)
            (define val-tmp (gensym 'val))
            (values #`(begin (define-symbolic* #,val-tmp integer?)
                             (assert (>= #,val-tmp 0))
                             (assert (< #,val-tmp #,(Enum-num-items self))))
                    #`(set! #,var #,val-tmp)
                    (list val-tmp)
                    (list self)
                    (list val-tmp))]

           [else
            (error (format "Unknown Enum update type: ~a~%"
                           update-type))]))])

;; Different Enums should not be able to replace each other. So, in
;; the subtype method we compare Enums by identity. However if the
;; programmer keeps creating new types by saying (Enum-type 10) over
;; and over instead of creating a single type and using it
;; consistently, then we would get weird behavior.
;; To prevent this, Enum-type expands to a define, so as to force the
;; programmer to give the type a name. This makes it obvious that they
;; are supposed to reuse that name in the program.
(define-syntax-rule (define-enum-type name items)
  (define-untyped-constant name (Enum 'name items)))

(struct Vectr (len input-type output-type)
  #:methods gen:Type
  [(define/generic generic-is-subtype? is-subtype?)
   (define/generic generic-repr repr)
   (define/generic generic-mutable-structure? mutable-structure?)
   (define/generic generic-symbolic-code symbolic-code)
   (define/generic generic-update-code update-code)
   (define/generic generic-old-values-code old-values-code)
   (define/generic generic-symbolic-update-code symbolic-update-code)
   
   (define (is-subtype? self other-type)
     (and (Vectr? other-type)
          (generic-is-subtype? (Vectr-input-type other-type)
                               (Vectr-input-type self))
          (generic-is-subtype? (Vectr-output-type self)
                               (Vectr-output-type other-type))))

   (define (repr self)
     `(Vectr ,(Vectr-len self)
              ,(generic-repr (Vectr-input-type self))
              ,(generic-repr (Vectr-output-type self))))


   (define (mutable-structure? self) #t)
   
   (define (symbolic-code self var varset-name)
     (define tmp (gensym))
     #`(define #,var
         (build-vector
          #,(Vectr-len self)
          (lambda (i)
            #,(generic-symbolic-code (Vectr-output-type self) tmp varset-name)
            #,tmp))))

   (define (update-code self update-type)
     (define output-type (Vectr-output-type self))
     (cond [(equal? update-type 'assign)
            (if (generic-mutable-structure? output-type)

                (lambda (vect index . args)
                  (apply (generic-update-code output-type update-type)
                         #`(vector-ref #,vect #,index)
                         args))

                (lambda (vect index value)
                  #`(vector-set! #,vect #,index #,value)))]

           [else
            (error (format "Unknown Vector update type: ~a~%"
                           update-type))]))

   (define (old-values-code self update-type var . update-args)
     (define output-type (Vectr-output-type self))
     (cond [(equal? update-type 'assign)
            (if (generic-mutable-structure? output-type)
                (apply generic-old-values-code
                       output-type
                       ;; TODO: update-type needs to change here
                       update-type
                       #`(vector-ref #,var #,(car update-args))
                       (cdr update-args))

                (let ([old-val-tmp (gensym 'old-value)])
                  (values
                   #`(define #,old-val-tmp (vector-ref #,var #,(car update-args)))
                   (list old-val-tmp)
                   (list output-type))))]
           
           [else
            (error (format "Unknown Vector update type: ~a~%"
                           update-type))]))

   (define (symbolic-update-code self update-type var)
     (define output-type (Vectr-output-type self))
     (cond [(and (equal? update-type 'assign)
                 (generic-mutable-structure? output-type))
            (define tmp-index (gensym 'index))
            (define tmp-val (gensym 'val))
            (let-values ([(output-defns output-update output-update-symbolic-vars output-update-symbolic-vars-types output-update-args)
                          (generic-symbolic-update-code
                           output-type
                           ;; TODO: update-type needs to change here
                           update-type
                           #`(vector-ref #,var #,tmp-index))])
              (values
               ;; Create the symbolic index into the vector
               #`(begin (define-symbolic* #,tmp-index integer?)
                        (assert (>= #,tmp-index 0))
                        (assert (< #,tmp-index #,(Vectr-len self)))
                        #,output-defns)
               ;; Update the mutable structure at the specified symbolic index
               output-update
               (cons tmp-index output-update-symbolic-vars)
               (cons (Vectr-input-type self) output-update-symbolic-vars-types)
               (cons tmp-index output-update-args)))]

           [(equal? update-type 'assign)
            (define tmp-index (gensym 'index))
            (define tmp-val (gensym 'val))
            (define old-val-tmp (gensym 'old-value))
            (values
             ;; Create the symbolic index into the vector
             #`(begin (define-symbolic* #,tmp-index integer?)
                      (assert (>= #,tmp-index 0))
                      (assert (< #,tmp-index #,(Vectr-len self)))
                      ;; Create the symbolic value
                      ;; TODO: This isn't an issue now, since output-type cannot be a mutable structure, but what if symbolic-code creates other symbolic variables besides tmp-val?
                      #,(generic-symbolic-code output-type tmp-val #f))
             ;; Perform the update
             #`(vector-set! #,var #,tmp-index #,tmp-val)
             (list tmp-index tmp-val)
             (list (Vectr-input-type self) output-type)
             (list tmp-index tmp-val))]

           [else
            (error (format "Unknown Vector update type: ~a~%"
                           update-type))]))])

(define (Vector-type length-or-input output)
  (unless (and (or (Enum? length-or-input)
                   (integer? length-or-input))
               (Type? output))
    (error (format "Invalid arguments to Vector-type: ~a and ~a~%" length-or-input output)))
  (define length
    (if (Enum? length-or-input)
        (Enum-num-items length-or-input)
        length-or-input))
  (define input
    (if (Enum? length-or-input)
        length-or-input
        (Integer-type)))
  (Vectr length input output))
