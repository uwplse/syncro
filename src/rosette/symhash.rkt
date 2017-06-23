#lang rosette

(require (only-in "util.rkt" maybe-internal-error))

(provide rhash rhash? rhash-has-key? rhash-ref rhash-set rhash-keys)

;; Implementation of hash maps that can handle symbolic keys and values.
;; Requires that keys do not *contain* symbolic values, and that keys
;; are not terms (however they can be symbolic unions).
;; Pretty stupid implementation. Would not work well if there's a lot
;; of symbolic keys or path conditions.
(define (rhash [assocs '()])
  (define result (hash))
  (let loop ([assocs assocs] [result result])
    (if (null? assocs)
        result
        (loop (cdr assocs)
              (rhash-set result (caar assocs) (cdar assocs))))))

(define (rhash? thing)
  (for/all ([thing thing])
    (hash? thing)))

(define (rhash-has-key? rhash key)
  (for*/all ([rhash rhash]
             [key key])
    (begin
      (unless (and (not (term? rhash)) (hash? rhash)
                   (not (term? key)))
        (maybe-internal-error
         (format "Invalid arguments to rhash-has-key?: ~a ~a" rhash key)))
      (hash-has-key? rhash key))))

(define (rhash-ref rhash key)
  (for*/all ([rhash rhash]
             [key key])
    (begin
      (unless (and (not (term? rhash)) (hash? rhash)
                   (not (term? key)))
        (maybe-internal-error
         (format "Invalid arguments to rhash-ref: ~a ~a" rhash key)))
      (hash-ref rhash key))))

(define (rhash-set rhash key value)
  (for*/all ([rhash rhash]
             [key key])
    (begin
      (unless (and (not (term? rhash)) (hash? rhash)
                   (not (term? key)))
        (maybe-internal-error
         (format "Invalid arguments to rhash-ref: ~a ~a" rhash key)))
      (hash-set rhash key value))))

(define (rhash-keys rhash)
  (for/all ([rhash rhash])
    (hash-keys rhash)))
