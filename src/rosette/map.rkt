#lang rosette

(require "util.rkt")

(provide build-map
         map-has-key? map-ref map-set!
         map-keys map-values map-on-map)

(struct vec-map (keys values) #:transparent)

(define (build-map capacity input-fn output-fn varset)
  (when (term? capacity)
    (internal-error
     (format "make-symbolic-map: capacity is not concrete: ~a" capacity)))

  (vec-map (for/vector #:length capacity ([i capacity])
             (input-fn i))
           (for/vector #:length capacity ([i capacity])
             (output-fn i))))

(define (map-has-key? map key)
  (for*/all ([keys (vec-map-keys map)]
             [capacity (vector-length keys)])
    (my-for/or ([i capacity])
      (equal? key (vector-ref keys i)))))

(define (map-ref map key)
  (for*/all ([keys (vec-map-keys map)]
             [capacity (vector-length keys)])
    (let* ([vals (vec-map-values map)])
      (my-for/or ([i capacity])
        (and (equal? key (vector-ref keys i))
             (vector-ref vals i))))))

;; Requires that the key is already in the map.
(define (map-set! map key val)
  ;; This is tricky. You are not supposed to use for/all to do
  ;; imperative stuff. Need to think carefully about how this should
  ;; be implemented.
  ;; Make sure that when modifying the vector you add a guard that
  ;; ensures that it retains the original value in cases where the
  ;; guard is not true.
  ;; TODO: Think about whether this makes sense taking path conditions
  ;; into account.
  (for*/all ([keys (vec-map-keys map)]
             [capacity (vector-length keys)])
    (for ([i capacity])
      (let* ([vals (vec-map-values map)]
             ;; TODO: Is this condition necessary?
             [condition (= capacity (vector-length (vec-map-keys map)))])
        (vector-set! vals i
                     (if (and condition (equal? (vector-ref keys i) key))
                         val
                         (vector-ref vals i)))))))

(define (map-keys map)
  (vector->list (vec-map-keys map)))

(define (map-values map)
  (vector->list (vec-map-values map)))

;; Actually want builtin map here
(define (map-on-map f vecmap)
  (map f (map-keys vecmap) (map-values map)))
