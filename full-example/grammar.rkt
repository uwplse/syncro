#lang s-exp rosette

(require rosette/lib/synthax)

(provide stmt-synthax generate-forms sketch
         vector-sum vector-increment! vector-decrement!)

(define (vector-sum vec)
  (define result 0)
  (for ([v vec])
    (set! result (+ result v)))
  result)

(define (vector-increment! vec index)
  (vector-set! vec index
               (+ (vector-ref vec index) 1)))

(define (vector-decrement! vec index)
  (vector-set! vec index
               (- (vector-ref vec index) 1)))

(define (sketch a b c)
  (display (format "Running sketch~%"))
  (stmt-synthax a b c 2))

;; TODO: Make this type directed, so that each terminal has a type
;; attached to it that determines where it can be used
(define-synthax (stmt-synthax terminal ... num)
  #:base (void)
  #:else (choose (void)
                 (begin (stmt-base-synthax terminal ... 2)
                        (stmt-synthax terminal ... (- num 1)))))

(define-synthax stmt-base-synthax
  ([(_ terminal ... num)
    (choose
     ([choose vector-increment! vector-decrement!] (numeric-vector-synthax terminal ... num)
                                                   (index-synthax terminal ... num))
     (vector-set! (numeric-vector-synthax terminal ... num)
                  (index-synthax terminal ... num)
                  (numeric-synthax terminal ... num)))]))
                
(define-synthax (numeric-vector-synthax terminal ... num)
  #:base (choose terminal ...)
  #:else (choose terminal ...))

(define-synthax (index-synthax terminal ... num)
  #:base (choose terminal ...)
  #:else (choose terminal ...))

(define-synthax (numeric-synthax terminal ... num)
  #:base (choose terminal ...)
  #:else (choose terminal ...))
