;; define-constant behaves just like define, but the system needs to
;; know types during grammar construction.
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

(define-incremental num1sum (Integer-type) (num1) ()
  (my-for/sum ([x num1]) x))

;; TODO: Annoying bitwidth stuff makes this tricky (see tmp3.rkt)
(define-incremental num2sum (Integer-type) (num2) ()
  (my-for/sum ([x num2]) x))

(define (value)
  (/ (* BETA num1sum) (+ (* BETA VOCABULARY_SIZE) num2sum)))

(define (go)
  (for/sum ([i 3])
    (assign-word->topic! (random NUM_WORDS) (random NUM_TOPICS))
    (value)))

(go)

;; TODO: Allow programmer-defined updates
;; TODO: Allow programmer-defined sketches for update rules
;; TODO: Here we assume that the programmer only uses our update
;; functions, like assign-word->topic!
;; We should not allow the programmer to do arbitrary updates (such as
;; just using vector-set!). We can get this behavior by encapsulating
;; the data structures in a class, and only providing certain kinds of
;; updates.
