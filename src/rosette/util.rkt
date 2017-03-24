#lang rosette

(provide display-errors? maybe-internal-error internal-error
         display-return
         input-val input-preconditions input?
         (rename-out [input make-input])
         my-for/sum my-for/or my-for/and coerce-evaluate clone)

(define display-errors? (make-parameter #f))
(define (maybe-internal-error str)
  (when (display-errors?)
    (displayln str))
  (error str))

(define (internal-error str)
  (display "INTERNAL ERROR: ")
  (displayln str)
  (error str))

(define (display-return x)
  (displayln x)
  x)

(struct input (val preconditions) #:transparent)

;; NOTE: The reimplementations of for can also be found in constructs.rkt
(define-syntax (my-for/sum stx)
  (syntax-case stx ()
    [(_ ([i itr] ...) expr ...)
     (with-syntax ([(itr-var ...) (generate-temporaries #'(i ...))])
       (syntax/loc stx
         (for*/all ([itr-var itr] ...)
           (let ([sum 0])
             (for ([i itr-var] ...)
               (set! sum (+ sum (begin expr ...))))
             sum))))]))

;; TODO: Not exactly semantically correct -- this will iterate through
;; the entire sequence, even if we could break somewhere in the
;; middle. Should be reimplemented with break. Also change in
;; constructs.rkt.
(define-syntax (my-for/or stx)
  (syntax-case stx ()
    [(_ ([i itr] ...) expr ...)
     (with-syntax ([(itr-var ...) (generate-temporaries #'(i ...))])
       (syntax/loc stx
         (for*/all ([itr-var itr] ...)
           (let ([val #f])
             (for ([i itr-var] ...)
               (set! val (or val (begin expr ...))))
             val))))]))

;; TODO: Not exactly semantically correct -- this will iterate through
;; the entire sequence, even if we could break somewhere in the
;; middle. Should be reimplemented with break. Also change in
;; constructs.rkt.
(define-syntax (my-for/and stx)
  (syntax-case stx ()
    [(_ ([i itr] ...) expr ...)
     (with-syntax ([(itr-var ...) (generate-temporaries #'(i ...))])
       (syntax/loc stx
         (for*/all ([itr-var itr] ...)
           (let ([val #t])
             (for ([i itr-var] ...)
               (set! val (and val (begin expr ...))))
             val))))]))

(define (coerce-evaluate thing model)
  (define sym-map
    (make-hash (map (lambda (sym) (cons sym sym))
                    (symbolics thing))))
  
  (evaluate thing (complete model sym-map)))

;; TODO: Lots of cases that aren't covered here, such as structs.
;; Probably also want to handle symbolic unions, which could be cyclic.
(define (clone thing)
  (match thing
    [(? list?)
     (map clone thing)]
    [(cons x y)               
     (cons (clone x) (clone y))]
    [(? vector?)              
     (for/vector #:length (vector-length thing) ([x thing])
       (clone x))]
    [(? box?)
     ((if (immutable? thing) box-immutable box) (clone (unbox thing)))]
    [_ thing]))
  

