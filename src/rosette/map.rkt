#lang rosette

(require "operators.rkt" "util.rkt")

(provide build-map
         map-has-key? map-ref map-set!
         map-keys map-values map-on-map)

;; A vec map must have a specific set of keys that it maps to
;; values. The value associated with a key can change, but the set of
;; keys itself must be specified initially and can never be changed.
(struct vec-map (keys values) #:transparent)

(define (build-map capacity input-fn output-fn [varset #f])
  (when (term? capacity)
    (internal-error
     (format "make-symbolic-map: capacity is not concrete: ~a" capacity)))

  (define keys
    (for/vector #:length capacity ([i capacity])
      (if varset (input-fn i varset) (input-fn i))))
  (define vals
    (for/vector #:length capacity ([i capacity])
      (if varset (output-fn i varset) (output-fn i))))

  ;; TODO(metasketch): Don't use an assert directly
  (assert (apply distinct? (vector->list keys)))
  (vec-map keys vals))

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
  ;; imperative stuff.
  ;; Since this is a vector, we can simply walk through the indices in
  ;; the vector and mutate it if the key matches. Rosette lifts
  ;; vectors so this works fine.
  ;; The only catch is how to walk through all the indices. A simple
  ;; solution is to figure out the maximum possible capacity over all
  ;; the potential maps, and then do all the indices up to that. This
  ;; may include some invalid indices for some maps, so we would have
  ;; to add a check to make sure the index is valid.
  (define keys (vec-map-keys map))
  (define vals (vec-map-values map))
  (define capacity (vector-length keys))
  (define max-capacity
    (if (union? keys)
        (apply max (map second (union-contents capacity)))
        capacity))

  (for ([i max-capacity])
    (when (and (< i capacity) (equal? (vector-ref keys i) key))
      (vector-set! vals i val))))

(define (map-keys map)
  (vector->list (vec-map-keys map)))

(define (map-values map)
  (vector->list (vec-map-values map)))

;; Actually want builtin map here
(define (map-on-map f vecmap)
  (map f (map-keys vecmap) (map-values vecmap)))
