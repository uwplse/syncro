#lang rosette

(require "../../src/rosette/util.rkt")

(provide (all-defined-out))

(define (is-wild? x)
  (and (not (symbolic? x)) (pair? x) (equal? (car x) 'wild)))

(define (wild-equal? x y h)
  (or (and (is-wild? x) #;(not (pair? y))
           (when (= (length x) 2)
             (hash-set! h (cadr x) (cons y (hash-ref h (cadr x) '()))))
           #t)
      (and (is-wild? y) #;(not (pair? x))
           (when (= (length y) 2)
             (hash-set! h (cadr y) (cons x (hash-ref h (cadr y) '()))))
           #t)
      (eq? x y)
      (and (pair? x) (pair? y)
           (= (length x) (length y))
           (not (member #f (map (lambda (a b) (wild-equal? a b h)) x y))))))
