#lang racket

(require "../../src/racket/constructs.rkt")

;; This problem can take quite a long time (almost 2 hours with
;; bitwidth of 10 and vector length of 10). However, making the vector
;; smaller makes it considerably faster.

(define-incremental nums (Vector-type 2 (Integer-type)) () (assign)
  (make-vector 2 0))

(define-incremental sum (Integer-type) (nums) ()
  (vector-sum nums))

;; Expected result:
;; (set! sum (+ sum (- val17042 old-value17044)))

;; Or, with more statements but smaller expression depth:
;; (set! sum (- sum old-value17044))
;; (set! sum (+ sum val17042)))

(finalize)

(displayln sum) ;; expect 0
(assign-nums! 0 8)
(assign-nums! 1 8)
(assign-nums! 5 5)
(displayln sum) ;; expect 21
(assign-nums! 0 5)
(assign-nums! 1 5)
(displayln sum) ;; expect 15
(assign-nums! 2 10)
(displayln sum) ;; expect 25
