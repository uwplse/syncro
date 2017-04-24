#lang incremental

(define NUM_WORDS (Integer-type) 12)
(define NUM_TOPICS (Integer-type) 3)
(define NUM_DOCUMENTS (Integer-type) 2)
(define VOCABULARY_SIZE (Integer-type) 10)
(define BETA (Integer-type) 1)

;; Enum types can be used as indices into vectors.
;; We assume that the words have already been interned.
(define-enum-type Word NUM_WORDS)
(define-enum-type Topic NUM_TOPICS)
(define-enum-type Document NUM_DOCUMENTS)

;; Vector mapping each word to the document it is in.
(define word->document (Vector-type Word Document)
  (build-vector NUM_WORDS (lambda (w) (if (< w 5) 0 1))))

;; Vector mapping each word to the topic it is currently assigned.
;; Every iteration one of the words has its topic changed.
;; The allowed mutations are "assign", meaning an assignment to one
;; slot in the vector.
(define-incremental word->topic (Vector-type Word Topic)
  #:initialize (build-vector NUM_WORDS (lambda (w) (random NUM_TOPICS)))
  #:updates
  [(change-topic! assign)
   
   ;; TODO: Allow programmer-defined updates, for example:
   #;(define (change-topic! word new-topic)
       (define old-topic (vector-ref word->topic word))
       (vector-set! word->topic word new-topic))])

(define-incremental num1 (Vector-type Topic (Integer-type))
  #:value
  (begin
    (build-vector
     NUM_TOPICS
     (lambda (t)
       (my-for/sum ([w NUM_WORDS])
                   (if (equal? (vector-ref word->topic w) t) 1 0)))))
  ;(size (set (Word w) | (equal? (vector-ref word->topic w) t)))))
  #:depends (word->topic))
;; Expect
;; (let ()
;;   (vector-decrement! num1 old-value)
;;   (vector-increment! num1 new-value))

(define-incremental num2helper (Vector-type Document (Vector-type Topic (Integer-type)))
  #:value
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
                          0)))))))
  ;; (size (set (Word w) | (and (equal? (vector-ref word->topic w) t)
  ;;                            (equal? (vector-ref word->document w) d)))
  #:depends (word->topic))
;; Expect
;; (let ()
;;   (define doc (vector-ref word->document index))
;;   (define subvec (vector-ref num2helper doc))
;;   (vector-decrement! subvec old-value)
;;   (vector-increment! subvec new-value))

(define-incremental num2 (Vector-type Document (Integer-type))
  #:value
  (build-vector
   NUM_DOCUMENTS
   (lambda (d)
     (my-for/sum ([t NUM_TOPICS])
                 (if (my-for/or ([w NUM_WORDS])
                                (and (equal? (vector-ref word->topic w) t)
                                     (equal? (vector-ref word->document w) d)))
                     1
                     0))))
  ;; (size (set (Topic t) | (exists (Word w)
  ;;                                (and (equal? (vector-ref word->topic w) t)
  ;;                                     (equal? (vector-ref word->document w) d))))))))
  #:depends (word->topic)
  #:sketches
  [(change-topic!
    (lambda (word new-topic)
      (if (equal? new-topic (??))
          (void)
          (begin
            (if (= (vector-ref
                    (vector-ref num2helper (vector-ref word->document word))
                    (??))
                   (??))
                (??)
                (??))
            (if (= (vector-ref
                    (vector-ref num2helper (vector-ref word->document word))
                    new-topic)
                   (??))
                (??)
                (??))))))])
;; Expect
;; (unless (equal? old-value new-value)
;;   (let ()
;;     (define doc (vector-ref word->document index))
;;     (define subvec (vector-ref num2helper doc))
;;     (when (= (vector-ref subvec new-value) 1)
;;       (vector-increment! num2 doc))
;;     (when (= (vector-ref subvec old-value) 0)
;;       (vector-decrement! num2 doc))))

(define-incremental num1sum (Integer-type)
  #:value (my-for/sum ([x num1]) x)
  #:depends (num1))
;; Expect
;; (let () (void))
;; (Turns out that num1 is guaranteed to be constant)

(define-incremental num2sum (Integer-type)
  #:value (my-for/sum ([x num2]) x)
  #:depends (num2))
;; Expect
;; (unless (equal? old-value new-value)
;;   (let ()
;;     (define doc (vector-ref word->document index))
;;     (define subvec (vector-ref num2helper doc))
;;     (when (= (vector-ref subvec new-value) 1)
;;       (set! num2sum (+ num2sum 1)))
;;     (when (= (vector-ref subvec old-value) 0)
;;       (set! num2sum (- num2sum 1)))))

(algorithm
 ;; TODO: Here we assume that the programmer only uses our update
 ;; functions, like assign-word->topic!
 ;; We should not allow the programmer to do arbitrary updates (such as
 ;; just using vector-set!). We can get this behavior by encapsulating
 ;; the data structures in a class, and only providing certain kinds of
 ;; updates.
 
 (define (value)
   (/ (* BETA num1sum) (+ (* BETA VOCABULARY_SIZE) (my-for/sum ([x num2]) x) #;num2sum)))
 
 (define (go)
   (for/sum ([i 3])
     (change-topic! (random NUM_WORDS) (random NUM_TOPICS))
     (value)))
 
 (go))
