#lang incremental

;; This problem can take quite a long time (almost 2 hours with
;; bitwidth of 10 and vector length of 10). However, making the vector
;; smaller makes it considerably faster.

(define int #:value (Integer-type) #:for-types)

(define-structure nums #:type (Vector-type 4 int)
  #:initialize (make-vector 4 0)
  #:deltas
  [(define (assign-nums! [idx int] [val int])
     (vector-set! nums idx val))])

(define-structure sum #:type int
  #:value (vector-sum nums)
  #:depends (nums))

;; Expected result:
;; (set! sum (+ sum (- val17042 old-value17044)))

;; Or, with more statements but smaller expression depth:
;; (set! sum (- sum old-value17044))
;; (set! sum (+ sum val17042)))

(algorithm
 (displayln sum) ;; expect 0
 (assign-nums! 0 8)
 (assign-nums! 1 8)
 (assign-nums! 3 5)
 (displayln sum) ;; expect 21
 (assign-nums! 0 5)
 (assign-nums! 1 5)
 (displayln sum) ;; expect 15
 (assign-nums! 2 10)
 (displayln sum)) ;; expect 25
