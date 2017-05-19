#lang incremental

(define int (Integer-type))
(define-symbolic NUM_STUDENTS #:type int #:configs [3 4])
(define-enum-type Student NUM_STUDENTS)

(define-symbolic PASSING_GRADE #:type int)

(define-record Student-info
  ;; Could add an id field and assume that the id is always equal to
  ;; the index into the vector to test assumes
  ;(id Student)
  ;; Students who get below a PASSING_GRADE will fail.
  (grade int)
  ;; Students could get an incomplete instead of failing.
  #;(incomplete (Boolean-type)))

(define-incremental info #:type (Vector-type Student Student-info-type)
  #:initialize (make-vector NUM_STUDENTS (Student-info 0))
  #:updates
  [(define (assign-info! [student Student] [new-info Student-info-type])
     (vector-set! info student new-info))])

(define-incremental failing-students #:type (Set-type Student)
  #:value
  (let ([result (enum-make-set NUM_STUDENTS)])
    (for ([student NUM_STUDENTS])
      (when (< (get-field (vector-ref info student) 'grade) PASSING_GRADE)
        (enum-set-add! result student)))
    result)
  #:depends (info))

;; Expected result:
;; (if (< (get-field new-value 'grade) PASSING_GRADE)
;;     (enum-set-add! failing-students index)
;;     (enum-set-remove! failing-students index))

(algorithm
 (displayln (enum-set-size passing-students)) ;; expect 0
 (assign-info! 0 (Student-info 8))
 (assign-info! 1 (Student-info 8))
 (assign-info! 2 (Student-info 5))
 (displayln (enum-set-size passing-students)) ;; expect 2
 (assign-info! 0 (Student-info 5))
 (assign-info! 1 (Student-info 5))
 (displayln (enum-set-size passing-students)) ;; expect 0
 (assign-info! 2 (Student-info 8))
 (displayln (enum-set-size passing-students))) ;; expect 1
