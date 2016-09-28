#lang racket

(require "constructs.rkt" "grammar.rkt")

(define-constant NUM_WORDS (Integer-type) 12)
(define-constant NUM_TOPICS (Integer-type) 3)
(define-constant NUM_DOCUMENTS (Integer-type) 2)
(define-constant VOCABULARY_SIZE (Integer-type) 10)
(define-constant BETA (Integer-type) 1)

(define-enum-type Word NUM_WORDS)
(define-enum-type Topic NUM_TOPICS)
(define-enum-type Document NUM_DOCUMENTS)

(define-constant word->document (Vector-type Word Document)
  (build-vector NUM_WORDS (lambda (w) (if (< w 5) 0 1))))

;; TODO: In the future, have programmer-defined updates
(define-incremental word->topic (Vector-type Word Topic) () (assign)
  (build-vector NUM_WORDS (lambda (w) (random NUM_TOPICS))))

(define-incremental num1 (Vector-type Topic (Integer-type)) (word->topic) ()
  (begin
    (build-vector
     NUM_TOPICS
     (lambda (t)
       (my-for/sum ([w NUM_WORDS])
                   (if (equal? (vector-ref word->topic w) t) 1 0))))))
  ;(size (set (Word w) | (equal? (vector-ref word->topic w) t))))))

(define-incremental num2helper (Vector-type Document (Vector-type Topic (Integer-type))) (word->topic) ()
  (begin
    (build-vector
     NUM_DOCUMENTS
     (lambda (d)
       (build-vector
        NUM_TOPICS
        (lambda (t)
          (my-for/sum ([w NUM_WORDS])
                      (if (and (equal? (vector-ref word->topic w) t)
                               (equal? (vector-ref word->document w) d))
                          1
                          0))))))))
;; (size (set (Word w) | (and (equal? (vector-ref word->topic w) t)
;;                            (equal? (vector-ref word->document w) d))))

(define-incremental num2 (Vector-type Document (Integer-type)) (word->topic) ()
  (build-vector
   NUM_DOCUMENTS
   (lambda (d)
     (my-for/sum ([t NUM_TOPICS])
       (if (my-for/or ([w NUM_WORDS])
             (and (equal? (vector-ref word->topic w) t)
                  (equal? (vector-ref word->document w) d)))
           1
           0)))))
     ;; (size (set (Topic t) | (exists (Word w)
     ;;                                (and (equal? (vector-ref word->topic w) t)
     ;;                                     (equal? (vector-ref word->document w) d))))))))

(define-incremental value (Integer-type) (num1 num2) ()
  (let ([num1sum (my-for/sum ([x num1]) x)]
        [num2sum (my-for/sum ([x num2]) x)])
    (/ (* BETA num1sum) (+ (* BETA VOCABULARY_SIZE) num2sum))))
;; (sum (/ (* BETA num1) (+ (* BETA VOCABULARY_SIZE) (sum num2))))))

(finalize)

(define (go)
  (for/sum ([i 3])
    ;; TODO: Here we assume that the programmer only uses our update functions, like assign-word->topic!
    ;; We should not allow the programmer to do arbitrary updates (such as just using vector-set!). We can get this behavior by encapsulating the data structures in a class, and only providing certain kinds of updates.
    (assign-word->topic! (random NUM_WORDS) (random NUM_TOPICS))
    value))

(go)
