#lang incremental

;; An implementation of LDA, following https://people.cs.umass.edu/~wallach/courses/s11/cmpsci791ss/readings/griffiths02gibbs.pdf

;;;;;;;;;;;;;;;
;; Constants ;;
;;;;;;;;;;;;;;;
(define NUM_WORD_INSTANCES (Integer-type) 12)
(define NUM_TOPICS (Integer-type) 3)
(define NUM_DOCUMENTS (Integer-type) 2)
(define VOCABULARY_SIZE (Integer-type) 10)
(define ALPHA (Integer-type) 1)
(define BETA (Integer-type) 1)

(define-enum-type WordInstance NUM_WORD_INSTANCES)
(define-enum-type WordText VOCABULARY_SIZE)
(define-enum-type Topic NUM_TOPICS)
(define-enum-type Document NUM_DOCUMENTS)

;; Vector mapping each word instance to the document it is in.
;; TODO: Can we make this symbolic but still constant?
(define word->document (Vector-type WordInstance Document)
  (build-vector NUM_WORD_INSTANCES (lambda (w) (if (< w 5) 0 1))))

;; Vector mapping each word instance to its identity.
;; TODO: Can we make this symbolic but still constant?
(define word->text (Vector-type WordInstance WordText)
  (build-vector NUM_WORD_INSTANCES (lambda (w) (remainder w VOCABULARY_SIZE))))

;; TODO: What happens if we write this as an incremental structure?
;; If we have the free variable analysis, it should automatically
;; realize that this is a constant
;; TODO: We actually always need values that are one smaller than the
;; values stored here. Writing that here would make the program less
;; clear. What performance benefit would it give?
(define num-for-document (Vector-type Document (Integer-type))
  (let ([result (make-vector NUM_DOCUMENTS 0)])
    (for ([w NUM_WORD_INSTANCES])
      (vector-increment! result (vector-ref word->document w)))
    result))

;;;;;;;;;;;;;;;;;;;;
;; Mutable values ;;
;;;;;;;;;;;;;;;;;;;;

;; Vector mapping each word to the topic it is currently assigned.
;; Every iteration one of the words has its topic changed.
(define-incremental word->topic (Vector-type WordInstance Topic)
  #:initialize (build-vector NUM_WORD_INSTANCES
                             (lambda (w) (random NUM_TOPICS)))
  #:updates [(change-topic! assign)])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Incremental structures ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-incremental num-for-topic-text (Vector-type Topic (Vector-type WordText (Integer-type)))
  #:value
  (let ([result (build-vector NUM_TOPICS
                              (lambda (topic)
                                (make-vector VOCABULARY_SIZE 0)))])
    (for ([w NUM_WORD_INSTANCES])
      (vector-increment! (vector-ref result (vector-ref word->topic w))
                         (vector-ref word->text w)))
    result)
  #:depends (word->topic))

;; Alternative slow definition for num-for-topic-text:
;; (build-vector
;;  NUM_TOPICS
;;  (lambda (topic)
;;    (build-vector
;;     VOCABULARY_SIZE
;;     (lambda (text)
;;       (my-for/sum ([w NUM_WORD_INSTANCES])
;;         (if (and (equal? (vector-ref word->topic w) topic)
;;                  (equal? (vector-ref word->text w) text))
;;             1
;;             0)))))))

(define-incremental num-for-topic (Vector-type Topic (Integer-type))
  #:value
  (build-vector
   NUM_TOPICS
   (lambda (topic)
     (vector-sum (vector-ref num-for-topic-text topic))))
  #:depends (word->topic))

(define-incremental num-for-document-topic (Vector-type Document (Vector-type Topic (Integer-type)))
  #:value
  (let ([result (build-vector NUM_DOCUMENTS
                              (lambda (doc)
                                (make-vector NUM_TOPICS 0)))])
    (for ([w NUM_WORD_INSTANCES])
      (vector-increment! (vector-ref result (vector-ref word->document w))
                         (vector-ref word->topic w)))
    result)
  #:depends (word->topic))

;; Alternative definition can be given as with num-for-topic-text

;;;;;;;;;;;;;;;
;; Algorithm ;;
;;;;;;;;;;;;;;;

(algorithm
 ;; A distribution over topics is a vector mapping each topic to a real number.
 ;; This corresponds to a probability distribution.
 ;; If the vector's sum is 1, then it is normalized.
 
 ;; Computes the distribution to sample from in the resampling step.
 (define (compute-unnormalized-distribution word)
   (let* ([word-text (vector-ref word->text word)]
          [word-topic (vector-ref word->topic word)]
          [doc (vector-ref word->document word)]
          ;; We  must exclude the current word, so we subtract 1
          [n-d-sum (- (vector-ref num-for-document doc) 1)])
     (build-vector
      NUM_TOPICS
      (lambda (topic)
        ;; We must exclude the current word, so we may need to subtract 1
        (let ([n-w-j (- (vector-ref (vector-ref num-for-topic-text topic) word-text)
                        (if (equal? topic word-topic) 1 0))]
              [n-sum-j (- (vector-ref num-for-topic topic)
                          (if (equal? topic word-topic) 1 0))]
              [n-d-j (- (vector-ref (vector-ref num-for-document doc) topic)
                        (if (equal? topic word-topic) 1 0))])
          ;; Copy the equation from the paper
          (* (/ (+ n-w-j BETA)
                (+ n-sum-j (* VOCABULARY_SIZE BETA)))
             (/ (+ n-d-j ALPHA)
                (+ n-d-sum (* NUM_TOPICS ALPHA)))))))))
 
 ;; Given a distribution over X, samples some X from that distribution.
 (define (sample distribution)
   (define rand (* (random) (vector-sum distribution)))
   (define sum 0)
   (for/last ([x (in-range (vector-length distribution))]
              #:break (>= sum rand))
     (set! sum (+ sum (vector-ref distribution x)))
     x))
 
 (define (main)
   (for ([iter (in-range 1000)])
     (for ([w (in-range NUM_WORD_INSTANCES)])
       (change-topic! w (sample (compute-unnormalized-distribution w)))))
   (displayln word->topic))
 
 (main))
