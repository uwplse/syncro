#lang incremental

(define SET_SIZE #:type (Integer-type) #:configs [5 3] #:for-types)
(define-enum-type SET_TYPE SET_SIZE)

(define-structure T? #:type (Set-type SET_TYPE)
  #:initialize (enum-make-set SET_SIZE)
  #:deltas
  [(define (add-elemT! [m SET_TYPE])
     (enum-set-add! T? m))

   #;(define (remove-elemT! [m SET_TYPE])
     (enum-set-remove! T? m))])

(define-structure S? #:type (Set-type SET_TYPE))

; (define-structure C? #:type (Set-type SET_TYPE)
;   #:value (enum-set-union T? S?)
;   #:depends (T? S?))

; (define-structure num-T #:type (Integer-type)
;   #:value (my-for/sum ([x SET_SIZE])
;             (if (enum-set-contains? T? x) 1 0))
;   #:depends (T?))

; (define-structure num-S #:type (Integer-type)
;   #:value (my-for/sum ([x SET_SIZE])
;             (if (enum-set-contains? S? x) 1 0))
;   #:depends (S?))

; (define-structure num-C #:type (Integer-type)
;   #:value (my-for/sum ([x SET_SIZE])
;             (if (enum-set-contains? C? x) 1 0))
;   #:depends (C?))

(algorithm
 (void))
