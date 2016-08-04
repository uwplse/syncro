#lang s-exp rosette

(require rosette/lib/synthax)
(require "enum-set.rkt" "synthesize-util.rkt" "util.rkt")

(provide BETA NUM_TOPICS NUM_DOCUMENTS NUM_WORDS word-document
         compute-num1 compute-set2 compute-num2 compute-tmp4 compute-value
         make-topic-symbol)

(current-bitwidth 5)

;;;;;;;;;;;;;;;
;; Constants ;;
;;;;;;;;;;;;;;;

(define BETA 2)
; Three topics, 0, 1 and 2
(define NUM_TOPICS 3)
; Two documents, 0 and 1
(define NUM_DOCUMENTS 2)
; Twelve words -- five in document 0 and seven in document 1
(define NUM_WORDS 12)
(define (word-document w)
  (if (< w 5) 0 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compute from scratch ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compute-num1)
  (define result (make-vector NUM_TOPICS 0))
  (for ([topic word-to-topic-map])
    (vector-increment! result topic))
  result)

(define (compute-set2)
  (define result (build-vector NUM_DOCUMENTS (lambda (i) (make-enum-set NUM_TOPICS))))

  (for ([word NUM_WORDS])
    (define doc (word-document word))
    (define topic (vector-ref word-to-topic-map word))
    (when (not (contained-in-vector? result doc topic))
      (add-in-vector! result doc topic)))

  result)

(define (compute-num2)
  (define result (make-vector NUM_DOCUMENTS '()))
  (for ([doc NUM_DOCUMENTS])
    (vector-set! result doc (enum-set-size (vector-ref set2 doc))))
  result)

(define (compute-tmp4)
  (+ (* BETA NUM_WORDS) (vector-sum num2)))

(define (compute-value)
  (* BETA (vector-sum num1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Incremental Function ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (change word new-topic)
  (define doc (word-document word))
  (define old-topic (vector-ref word-to-topic-map word))
  (vector-set! word-to-topic-map word new-topic)

  (vector-decrement! num1 old-topic)
  (set! value (- value BETA))

  (vector-increment! num1 new-topic)
  (set! value (+ value BETA))

  (unless (contained-in-vector? set2 doc new-topic)
    (add-in-vector! set2 doc new-topic)
    (vector-increment! num2 doc)
    (set! tmp4 (+ tmp4 1))
    (set! value (compute-value)))

  (define flag #t)
  (for ([w NUM_WORDS])
    (when (and (equal? (word-document w) doc)
               (equal? (vector-ref word-to-topic-map w) old-topic))
      (set! flag #f)))
  (when flag
    (remove-in-vector! set2 doc old-topic)
    (vector-decrement! num2 doc)
    (set! tmp4 (- tmp4 1))
    (set! value (compute-value))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Check for validity ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define (check)
  (assert (equal? set2 (compute-set2)))
  (assert (equal? num1 (compute-num1)))
  (assert (equal? num2 (compute-num2)))
  (assert (equal? tmp4 (compute-tmp4)))
  (assert (equal? value (compute-value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup and Incrementalization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-topic-symbol [dummy 0])
  (define-symbolic* x integer?)
  (assert (>= x 0))
  (assert (< x NUM_TOPICS))
  x)

(define word-to-topic-map (build-vector NUM_WORDS make-topic-symbol))
(define num1 (compute-num1))
(define set2 (compute-set2))
(define num2 (compute-num2))
(define tmp4 (compute-tmp4))
(define value (compute-value))

(define-symbolic word-to-change integer?)
(assert (>= word-to-change 0))
(assert (< word-to-change NUM_WORDS))
(define-symbolic topic-to-change integer?)
(assert (>= topic-to-change 0))
(assert (< topic-to-change NUM_TOPICS))

; Verification:
; (time
;   (with-handlers ([exn:fail? (lambda (ex) "Prevent the exception")])
;     (go)))
; When using lists as sets:
; cpu time: 10768 real time: 48582 gc time: 1146
; When using enum-sets for sets:
; cpu time: 444 real time: 19451 gc time: 60
(define (go)
  (time
   (verify (begin (change word-to-change topic-to-change) (check)))))

(define (model1)
  ; cpu time: 238 real time: 1711 gc time: 82
  (time
   (synthesize #:forall `(,word-to-change ,topic-to-change ,@(vector->list word-to-topic-map))
               #:guarantee (begin (vector-set! word-to-topic-map word-to-change topic-to-change)
                                  (stmt 3 3
                                        (make-hash `((numeric-vector . (,num1))
                                                     (topic . (,topic-to-change
                                                               ,(vector-ref word-to-topic-map word-to-change))))))
                                  (assert (equal? num1 (compute-num1)))))))

(define (model2)
  (let ([num1 num1] [new-topic topic-to-change] [old-topic (vector-ref word-to-topic-map word-to-change)])
    (time
     (synthesize #:forall `(,word-to-change ,topic-to-change ,@(vector->list word-to-topic-map))
                 #:guarantee (begin (vector-set! word-to-topic-map word-to-change topic-to-change)
                                    (stmt-synthax num1 new-topic old-topic 3)
                                    (assert (equal? num1 (compute-num1))))))))

(go)
