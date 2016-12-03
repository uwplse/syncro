#lang rosette/safe

;; Note: We use rosette/safe, but error is *not* in safe Rosette.
(require (only-in racket error))

(provide make-alist alist-copy alist-insert! alist-has-key?
         alist-get alist-get-and-remove!)

(struct no-value ())

(define (make-alist)
  (box '()))

(define (alist-copy alist)
  (box (unbox alist)))

(define (alist-insert! alist key value)
  (set-box! alist
            (cons (cons key value) (unbox alist))))

(define (alist-has-key? alist key)
  (define (loop lst)
    (and (not (null? lst))
         (or (equal? key (caar lst))
             (loop (cdr lst)))))
  (loop (unbox alist)))

(define (alist-get alist key)
  (define (loop lst)
    (cond [(null? lst)
           (error (format "Get: No such key ~a" key))]
          [(equal? key (caar lst))
           (cdar lst)]
          [else
           (loop (cdr lst))]))
  (loop (unbox alist)))

(define (alist-get-and-remove! alist key)
  (define (loop lst)
    (cond [(equal? key (caar lst))
           (cdr lst)]
          [else
           (cons (car lst) (loop (cdr lst)))]))

  (define result (alist-get alist key))
  (set-box! alist (loop (unbox alist)))
  result)
