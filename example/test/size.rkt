#lang incremental

(define-structure mystery? #:type (Vector-type 5 (Boolean-type))
  #:initialize (vector #t #f #f #t #f)
  #:deltas
  [(define (assign-mystery! [idx (Integer-type)] [m (Boolean-type)])
     (vector-set! mystery? idx m))])

(define-structure num-mysteries #:type (Integer-type)
  #:value (my-for/sum ([x mystery?])
            (if x 1 0))
  #:depends (mystery?))
 
;; Possible results:
;; (if old-value17044
;;     (set! num-mysteries (- num-mysteries 1))
;;     (void))
;; (if val17042
;;     (set! num-mysteries (+ num-mysteries 1))
;;     (void))

;; Or perhaps:
;; (if (and new-value (not old-value))
;;     (set! num-mysteries (+ num-mysteries 1))
;;     (void))
;; (if (and (not new-value) old-value)
;;     (set! num-mysteries (- num-mysteries 1))
;;     (void))

;; Or, if we have guards:
;; (if (equal? val17042 old-value17044)
;;     (void)
;;     (if val17042
;;         (set! num-mysteries (+ num-mysteries 1))
;;         (set! num-mysteries (- num-mysteries 1))))

(algorithm
 (displayln num-mysteries) ;; expect 2
 (assign-mystery?! 0 #t)
 (displayln num-mysteries) ;; expect 2
 (assign-mystery?! 3 #f)
 (displayln num-mysteries) ;; expect 1
 (assign-mystery?! 2 #t)
 (displayln num-mysteries)) ;; expect 2
