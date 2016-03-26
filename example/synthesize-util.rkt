#lang s-exp rosette

(require rosette/lib/synthax)
(require "util.rkt")

(provide stmt stmt-synthax)

;; Introduces a choice hole into a program -- a placeholder
;; to be filled with one of the given expressions.
;; Behaves like the choose macro in rosette/lib/meta/meta,
;; except it uses define-symbolic* instead of define-symbolic.
#;(define-syntax (choose stx)
  (syntax-case stx ()
    [(_ expr ... exprn)
     (with-syntax ([(choice ...) (generate-temporaries #'(expr ...))])
       (syntax/loc stx
         (begin
           (define-symbolic* choice boolean?) ...
           (cond [choice expr] ...
                 [else exprn]))))]))

(define (choose-fn val . vals)
  (define (loop vals)
    (if (null? (cdr vals))
        (car vals)
        (begin (define-symbolic* choice boolean?)
               (if choice
                   (car vals)
                   (loop (cdr vals))))))
  (loop (cons val vals)))

(define (stmt stmt-num expr-num variables)
  (if (= stmt-num 1)
      (stmt-base expr-num variables)
      (choose (stmt-base expr-num variables)
              (begin (stmt-base expr-num variables)
                     (stmt (- stmt-num 1) expr-num variables)))))

(define (stmt-base num variables)
  (choose
   ;; TODO: Vector and index should be chosen together
   (vector-increment! (numeric-vector-expr num variables)
                      (index-expr num variables))
   (vector-decrement! (numeric-vector-expr num variables)
                      (index-expr num variables))
   (vector-set! (numeric-vector-expr num variables)
                (index-expr num variables)
                (numeric-expr num variables))
   ;; TODO: Every loop variable will be called i. Will this lead to name clashes in nested loops?
   ;(for ([i NUM_THINGS])
     ;(stmt 3 expr-num variables))
   ))

(define (numeric-vector-expr num variables)
  (apply choose-fn (hash-ref variables 'numeric-vector)))

(define (index-expr num variables)
  (apply choose-fn (hash-ref variables 'topic)))

(define (numeric-expr num variables)
  0)

(define-synthax (stmt-synthax terminal ... num)
  #:base (void)
  #:else (choose (stmt-base-synthax terminal ... 2)
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
