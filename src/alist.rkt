#lang rosette

(provide make-alist alist-insert alist-has-key? alist-get alist-get-and-remove)

(struct no-value ())

;; TODO: Put this in a mutable box to make code nicer
(define (make-alist)
  '())

(define (alist-insert alist key value)
  (cons (cons key value) alist))

(define (alist-has-key? alist key)
  (and (not (null? alist))
       (or (equal? key (caar alist))
           (alist-has-key? (cdr alist) key))))

(define (alist-get alist key)
  (cond [(null? alist)
         (error (format "No such key ~a" key))]
        [(equal? key (caar alist))
         (cdar alist)]
        [else
         (alist-get (cdr alist) key)]))

(define (alist-get-and-remove alist key)
  (cond [(null? alist)
         (error (format "No such key ~a" key))]
        [(equal? key (caar alist))
         (list (cdar alist)
               (cons (cons (no-value) (no-value))
                     (cdr alist)))]
        [else
         (match-let ([(list result new-alist)
                      (alist-get-and-remove (cdr alist) key)])
           (list result (cons (car alist) new-alist)))]))
