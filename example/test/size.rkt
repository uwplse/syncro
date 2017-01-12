#lang racket

(require "../../src/racket/constructs.rkt")

(define-incremental mystery? (Vector-type 5 (Boolean-type)) () (assign)
  (vector #t #f #f #t #f))

(define-incremental num-mysteries (Integer-type) (mystery?) ()
  (my-for/sum ([x mystery?])
    (if x 1 0)))

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

(finalize)

(displayln num-mysteries) ;; expect 2
(assign-mystery?! 0 #t)
(displayln num-mysteries) ;; expect 2
(assign-mystery?! 3 #f)
(displayln num-mysteries) ;; expect 1
(assign-mystery?! 2 #t)
(displayln num-mysteries) ;; expect 2
