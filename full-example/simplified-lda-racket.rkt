#lang racket

;(require "rosette-namespace.rkt")
;(require "lsl-namespace.rkt")

(require "constructs.rkt")

;; Goal is for the following program to work:
(define-constant NUM_WORDS 12)
(define-constant NUM_TOPICS 3)
(define-constant NUM_DOCUMENTS 2)
(define-constant VOCABULARY_SIZE 10)
(define-constant BETA 1)
(define-constant word->document
  (build-vector NUM_WORDS (lambda (w) (if (< w 5) 0 1))))

;; TODO: All five of the above may need to use a "define-constant"
;; form that may look something like this:
;; This could help with doing things symbolically.
;; (define-constant word->document (Vector-type Word Document)
;;   (build-vector NUM_WORDS (lambda (w) (if (< w 5) 0 1))))

(define Word (Enum-type NUM_WORDS))
(define Topic (Enum-type NUM_TOPICS))
(define Document (Enum-type NUM_DOCUMENTS))

(define-incremental word->topic (Vector-type Word Topic) () (assign)
  (build-vector NUM_WORDS (lambda (w) (random NUM_TOPICS))))

(define-incremental num1 (Vector-type Topic (Integer-type)) (word->topic) ()
  (build-vector
   NUM_TOPICS
   (lambda (t)
     (my-for/sum ([w NUM_WORDS])
       (if (equal? (vector-ref word->topic w) t) 1 0)))))
     ;(size (set (Word w) | (equal? (vector-ref topics w) t))))))

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
  (lambda ()
    (let ([num1sum (for/sum ([x num1]) x)]
          [num2sum (for/sum ([x num2]) x)])
     (/ (* BETA num1sum) (+ (* BETA VOCABULARY_SIZE) num2sum)))))
    ;(sum (/ (* BETA num1) (+ (* BETA VOCABULARY_SIZE) (sum num2))))))

(finalize)

(define (go)
  (for/sum ([i 100])
    (assign-word->topic! (random NUM_WORDS) (random NUM_TOPICS))
    (value)))

(go)
