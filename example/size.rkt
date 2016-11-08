#lang racket

(require "../src/constructs.rkt" "../src/grammar.rkt")

(define-incremental mystery? (Vector-type 5 (Boolean-type)) () (assign)
  (vector #t #f #f #t #f))

(define-incremental num-mysteries (Integer-type) (mystery?) ()
  (my-for/sum ([x mystery?])
    (if x 1 0)))

;; Expected result:
;; (if (and new-value (not old-value))
;;     (set! num-mysteries (+ num-mysteries 1))
;;     (void))
;; (if (and (not new-value) old-value)
;;     (set! num-mysteries (- num-mysteries 1))
;;     (void))

(finalize)

(displayln num-mysteries) ;; expect 2
(assign-mystery?! 0 #t)
(displayln num-mysteries) ;; expect 2
(assign-mystery?! 3 #f)
(displayln num-mysteries) ;; expect 1
(assign-mystery?! 2 #t)
(displayln num-mysteries) ;; expect 2
