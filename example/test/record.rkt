#lang incremental

(define-enum-type Student 3)

(define-record Student-info
  ;; Could add an id field and assume that the id is always equal to
  ;; the index into the vector to test assumes
  ;(id Student)
  ;; Students who get below a 7 will by default fail.
  (grade (Integer-type))
  ;; Students could get an incomplete instead of failing.
  #;(incomplete (Boolean-type)))

(define-incremental info (Vector-type Student Student-info-type)
  #:initialize (make-vector 3 (Student-info 0))
  #:updates [(assign-info! assign)])

(define-incremental failing-students (Set-type Student)
  #:value
  (let ([result (enum-make-set 3)])
    (for ([student 3])
      (when (< (get-field (vector-ref info student) 'grade) 7)
        (enum-set-add! result student)))
    result)
  #:depends (info))

;; Expected result:
;; (if (< (get-field new-value 'grade) 7)
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
