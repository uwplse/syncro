#lang racket

(require "../../src/constructs.rkt" "../../src/grammar.rkt")

(define-enum-type Student 3)

(define-incremental grades (Vector-type Student (Integer-type)) () (assign)
  (make-vector 3 0))

(define-incremental passing-students (Set-type Student) (grades) ()
  (let ([result (enum-make-set 3)])
    (for ([student 3])
      (when (>= (vector-ref grades student) 7)
        (enum-set-add! result student)))
    result))

;; Expected result:
;; (if (< new-value 7)
;;     (enum-set-remove! passing-students index)
;;     (enum-set-add! passing-students index)

(finalize)

(displayln (enum-set-size passing-students)) ;; expect 0
(assign-grades! 0 8)
(assign-grades! 1 8)
(assign-grades! 2 5)
(displayln (enum-set-size passing-students)) ;; expect 2
(assign-grades! 0 5)
(assign-grades! 1 5)
(displayln (enum-set-size passing-students)) ;; expect 0
(assign-grades! 2 8)
(displayln (enum-set-size passing-students)) ;; expect 1
