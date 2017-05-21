#lang incremental

(define int #:value (Integer-type) #:for-types)
(define NUM_STUDENTS #:type int #:configs [3 4] #:for-types)
(define-enum-type Student NUM_STUDENTS)

(define PASSING_GRADE #:type int)

(define-structure grades #:type (Vector-type Student int)
  #:initialize (make-vector NUM_STUDENTS 0)
  #:deltas
  [(define (assign-grade! [student Student] [new-grade int])
     (vector-set! grades student new-grade))])

(define-structure passing-students #:type (Set-type Student)
  #:value
  (let ([result (enum-make-set NUM_STUDENTS)])
    (for ([student NUM_STUDENTS])
      (when (>= (vector-ref grades student) PASSING_GRADE)
        (enum-set-add! result student)))
    result)
  #:depends (grades))

;; Expected result:
;; (if (< new-value PASSING_GRADE)
;;     (enum-set-remove! passing-students index)
;;     (enum-set-add! passing-students index)

(algorithm
 (displayln (enum-set-size passing-students)) ;; expect 0
 (assign-grade! 0 8)
 (assign-grade! 1 8)
 (assign-grade! 2 5)
 (displayln (enum-set-size passing-students)) ;; expect 2
 (assign-grade! 0 5)
 (assign-grade! 1 5)
 (displayln (enum-set-size passing-students)) ;; expect 0
 (assign-grade! 2 8)
 (displayln (enum-set-size passing-students))) ;; expect 1
