#lang incremental

(define-enum-type Student 3)

(define-incremental grades (Vector-type Student (Integer-type))
  #:initialize (make-vector 3 0)
  #:updates [(define (swap-grades! [student1 Student] [student2 Student])
               (let ([tmp (vector-ref grades student1)])
                 (vector-set! grades student1 (vector-ref grades student2))
                 (vector-set! grades student2 tmp)))])

(define-incremental passing-students (Set-type Student)
  #:value
  (let ([result (enum-make-set 3)])
    (for ([student 3])
      (when (>= (vector-ref grades student) 7)
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
;; Could also directly check if the grade is under 7

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
