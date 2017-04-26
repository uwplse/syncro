#lang incremental

(define-enum-type Student 3)

(define-incremental grades (Vector-type Student (Integer-type))
  #:initialize (make-vector 3 0)
  #:updates
  [(define (assign-grade! [student Student] [new-grade (Integer-type)])
     (vector-set! grades student new-grade))])

(define-incremental passing-students (Set-type Student)
  #:value
  (let ([result (enum-make-set 3)])
    (for ([student 3])
      (when (>= (vector-ref grades student) 7)
        (enum-set-add! result student)))
    result)
  #:depends (grades)
  #:sketches
  [(assign-grade!
    (lambda (student new-grade)
      (if (< new-grade 7)
          (??)
          (enum-set-add! passing-students student))))])

;; Expected result:
;; (if (< new-value 7)
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
