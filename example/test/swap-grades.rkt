#lang incremental

(define int (Integer-type))

(define-symbolic NUM_STUDENTS #:type int #:configs [3 4])
(define-enum-type Student NUM_STUDENTS)

(define-symbolic PASSING_GRADE #:type int)

(define-incremental grades #:type (Vector-type Student int)
  #:initialize (make-vector NUM_STUDENTS 0)
  #:updates [(define (swap-grades! [student1 Student] [student2 Student])
               (let ([tmp (vector-ref grades student1)])
                 (vector-set! grades student1 (vector-ref grades student2))
                 (vector-set! grades student2 tmp)))])

(define-incremental passing-students #:type (Set-type Student)
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
