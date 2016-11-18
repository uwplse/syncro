#lang rosette/safe

;; Note: Although we use rosette/safe, internal-error uses error which
;; is *not* in safe Rosette.
(require (only-in "util.rkt" internal-error))

(provide make-alist alist-copy alist-insert! alist-has-key?
         alist-get alist-get-and-remove!)

(struct no-value ())

;; TODO: Put this in a mutable box to make code nicer
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
           (internal-error (format "No such key ~a" key))]
          [(equal? key (caar lst))
           (cdar lst)]
          [else
           (loop (cdr lst))]))
  (loop (unbox alist)))

(define (alist-get-and-remove! alist key)
  ;; Returns a list of two values -- the result of (alist-get alist
  ;; key), and the new alist after removing that entry.
  (define (loop lst)
    (cond [(null? lst)
           (internal-error (format "No such key ~a" key))]
          [(equal? key (caar lst))
           (list (cdar lst)
                 (cons (cons (no-value) (no-value))
                       (cdr lst)))]
          [else
           (let ([result-pair (loop (cdr lst))])
             (list (first result-pair)
                   (cons (car lst) (second result-pair))))]))

  (let ([result-pair (loop (unbox alist))])
    (set-box! alist (second result-pair))
    (first result-pair)))
