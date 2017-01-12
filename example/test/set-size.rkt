#lang racket

(require "../../src/racket/constructs.rkt")

(define-enum-type Mystery 5)

(define-incremental mystery? (Set-type Mystery) () (add remove)
  (make-enum-set))

(define-incremental num-mysteries (Integer-type) (mystery?) ()
  (my-for/sum ([x 5])
    (if (enum-set-contains? mystery? x) 1 0)))

;; Expected result (add):
;; (if old-value
;;     (void)
;;     (set! num-mysteries (+ num-mysteries 1)))
;; Expected result (remove):
;; (if old-value
;;     (set! num-mysteries (- num-mysteries 1))
;;     (void))

(finalize)

(displayln num-mysteries) ;; expect 0
(add-mystery?! 0)
(add-mystery?! 3)
(displayln num-mysteries) ;; expect 2
(add-mystery?! 0)
(displayln num-mysteries) ;; expect 2
(remove-mystery?! 3)
(displayln num-mysteries) ;; expect 1
(add-mystery?! 2)
(displayln num-mysteries) ;; expect 2
(remove-mystery?! 4)
(displayln num-mysteries) ;; expect 2
