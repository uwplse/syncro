#lang incremental

(define int #:value (Integer-type) #:for-types)

(define NUM_STUDENTS #:type int #:configs [3 4] #:for-types)
(define-enum-type Student NUM_STUDENTS)

(define PASSING_GRADE #:type int)

(define-structure grades #:type (Vector-type Student int)
  #:initialize (make-vector NUM_STUDENTS 0)
  #:deltas [(define (swap-grades! [student1 Student] [student2 Student])
               (let ([tmp (vector-ref grades student1)])
                 (vector-set! grades student1 (vector-ref grades student2))
                 (vector-set! grades student2 tmp)))])

(define-structure passing-students #:type (Set-type Student)
  #:value
  (let ([result (enum-make-set NUM_STUDENTS)])
    (for ([student NUM_STUDENTS])
      (when (>= (vector-ref grades student) PASSING_GRADE)
        (enum-set-add! result student)))
    result)
  #:depends (grades))

;; Expected result:
;; (let ()
;;   (define s1-pass? (enum-set-contains? passing-students student1))
;;   (define s2-pass? (enum-set-contains? passing-students student2))
;;   (if s1-pass?
;;       (enum-set-add! passing-students student2)
;;       (enum-set-remove! passing-students student2))
;;   (if s2-pass?
;;       (enum-set-add! passing-students student1)
;;       (enum-set-remove! passing-students student1))
;; Could also directly check if the grade is under PASSING_GRADE

;; Result the synthesizer gave me was basically that:
;; (let ()
;;   (define tmp38378 (enum-set-contains? passing-students student1))
;;   (let ()
;;     (if (enum-set-contains? passing-students student2)
;;         (enum-set-add! passing-students student1)
;;         (enum-set-remove! passing-students student1))
;;     (if (not tmp38378)
;;         (enum-set-remove! passing-students student2)
;;         (enum-set-add! passing-students student2))))

(algorithm
 ;; TODO
 (void))
