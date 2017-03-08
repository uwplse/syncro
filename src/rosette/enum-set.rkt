#lang rosette

(require "util.rkt")

(provide enum-make-set build-enum-set enum-make-symbolic-set
         enum-set-add! enum-set-remove! enum-set-contains?
         #;enum-set-empty? enum-set-size
         enum-set-union enum-set->list for-enum-set)

(define (enum-make-set num-things)
  (when (term? num-things)
    (internal-error
     (format "enum-make-set: num-things is not concrete: ~a" num-things)))
  (make-vector num-things #f))

(define (build-enum-set num-things fn)
  (for/all ([num-things num-things])
    (for/vector #:length num-things ([i num-things])
      (fn i))))

(define (enum-make-symbolic-set num-things [varset #f])
  (when (term? num-things)
    (internal-error
     (format "enum-make-set: num-things is not concrete: ~a" num-things)))
  (for/vector #:length num-things ([i num-things])
    (define-symbolic* choice boolean?)
    (when varset (set-add! varset (make-input choice '())))
    choice))

;; TODO: Rename to enum-set-member?
(define (enum-set-contains? set elem)
  (vector-ref set elem))

(define (enum-set-add! set elem)
  (vector-set! set elem #t))

(define (enum-set-remove! set elem)
  (vector-set! set elem #f))

;; TODO: Will for/and work, even if set is concrete?
(define (enum-set-empty? set)
  (for/and ([x set]) x))

(define (enum-set-size set)
  (define result 0)
  (for-enum-set ([x set])
    (set! result (+ result 1)))
  result)

(define (enum-set-union set1 . others)
  (for/all ([set1 set1])
    (let* ([num-items (vector-length set1)]
           [result (enum-make-set num-items)]
           [all (cons set1 others)])
      (for ([elem num-items])
        (when (ormap (lambda (x) (enum-set-contains? x elem)) all)
          (enum-set-add! result elem)))
      result)))

(define (enum-set->list set)
  (filter (lambda (elem) (enum-set-contains? set elem))
          (range (vector-length set))))

;; Semantics are redefined in grammar/language.rkt
;; Syntax is redefined in grammar/language.rkt and grammar/sketch.rkt
(define-syntax-rule (for-enum-set ([var set-expr]) body ...)
  (begin
    (define set set-expr)
    (define num-items (vector-length set))
    (when (term? num-items)
      (internal-error
       (format "for-enum-set: Number of items in enum set should be concrete, was ~a"
               num-items)))

    (for ([var num-items])
      (when (enum-set-contains? set var)
        body ...))
    (void)))
