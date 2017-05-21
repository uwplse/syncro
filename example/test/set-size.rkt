#lang incremental

(define NUM_MYSTERIES #:type (Integer-type) #:configs [5 3] #:for-types)
(define-enum-type Mystery NUM_MYSTERIES)

(define-structure mystery? #:type (Set-type Mystery)
  #:initialize (enum-make-set NUM_MYSTERIES)
  #:deltas
  [(define (add-mystery! [m Mystery])
     (enum-set-add! mystery? m))
   (define (remove-mystery! [m Mystery])
     (enum-set-remove! mystery? m))])

(define-structure num-mysteries #:type (Integer-type)
  #:value (my-for/sum ([x NUM_MYSTERIES])
            (if (enum-set-contains? mystery? x) 1 0))
  #:depends (mystery?))

;; Expected result (add):
;; (if old-value
;;     (void)
;;     (set! num-mysteries (+ num-mysteries 1)))
;; Expected result (remove):
;; (if old-value
;;     (set! num-mysteries (- num-mysteries 1))
;;     (void))

(algorithm
 (displayln num-mysteries) ;; expect 0
 (add-mystery! 0)
 (add-mystery! 3)
 (displayln num-mysteries) ;; expect 2
 (add-mystery! 0)
 (displayln num-mysteries) ;; expect 2
 (remove-mystery! 3)
 (displayln num-mysteries) ;; expect 1
 (add-mystery! 2)
 (displayln num-mysteries) ;; expect 2
 (remove-mystery! 4)
 (displayln num-mysteries)) ;; expect 2
