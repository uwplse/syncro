#lang rosette

(require "util.rkt")

(provide make-map make-symbolic-map
         map-has-key? map-ref map-set!
         map-keys map-values)

;; The keys and values in a vec-map may contain garbage.
;; Any time you use a key or a value from keys/values, you *must* make
;; sure it is valid by comparing the index against num-operations.
(struct vec-map ([num-operations #:mutable] keys values) #:transparent)

;; Capacity is a limit on the number of map-sets that can be performed
;; on the map. It is *not* a limit on how many elements can be in the
;; map at a time, it is *more* restrictive than that.
;; If we want to only have a limit on how many elements can be in the
;; map at a time, we would have to rewrite map-set! so that it
;; overwrites duplicate keys. Currently it simply adds it to the last
;; slot in the array, so it works like a log of operations.
(define (make-map capacity)
  (vec-map 0 (make-vector capacity #f) (make-vector capacity #f)))

(define (make-symbolic-map capacity input-fn output-fn varset)
  (when (term? capacity)
    (internal-error
     (format "make-symbolic-map: capacity is not concrete: ~a" capacity)))

  (define-symbolic* num-ops integer?)
  (when varset
    (define assertions (list (>= num-ops 0) (<= num-ops capacity)))
    (set-add! varset (make-input num-ops assertions)))

  (vec-map num-ops
           (for/vector #:length capacity ([i capacity]) (input-fn))
           (for/vector #:length capacity ([i capacity]) (output-fn))))

;; Depends on the fact that there is no delete-key.
(define (map-has-key? map key)
  ;; Use for/all to guarantee that (vector-length keys) will be concrete.
  (for/all ([keys (vec-map-keys map)])
    (my-for/or ([i (vector-length keys)])
      (and (< i (vec-map-num-operations map))
           (equal? key (vector-ref keys i))))))

(define (map-ref map key)
  ;; Use for/all to guarantee that capacity will be concrete.
  (for/all ([keys (vec-map-keys map)])
    (let* ([capacity (vector-length keys)]
           [vals (vec-map-values map)]
           [num-ops (vec-map-num-operations map)])
      (my-for/or ([i capacity])
        ;; Iterate backwards so you hit later transactions first
        (let ([index (- capacity i 1)])
          (and (< index num-ops)
               (equal? key (vector-ref keys index))
               (vector-ref vals index)))))))

(define (map-set! map key val)
  ;; This is tricky. You are not supposed to use for/all to do
  ;; imperative stuff.
  ;; Potential method: Figure out (an overapproximation of) all
  ;; possible values that num-operations could be, and for each such
  ;; value v add (when (= num-operations v) (vector-set! keys v key))
  ;; and so on.
  ;; Here, num-operations can be between 0 and max capacity - 1. (If
  ;; it's equal to the capacity, then map-set! should error.)
  (let* ([num-ops (vec-map-num-operations map)]
         [keys (vec-map-keys map)]
         [vals (vec-map-values map)]
         [capacity (vector-length keys)]
         [max-capacity
          (if (union? capacity)
              (apply max (map second (union-contents capacity)))
              capacity)])

    (define (set-at-location! index)
      (when (= index capacity)
        (maybe-internal-error "Map is full!"))

      (vector-set! keys index key)
      (vector-set! vals index val))

    (if (term? num-ops)
        (for ([possible-num-ops (+ 1 max-capacity)])
          (when (= possible-num-ops num-ops)
            (set-at-location! possible-num-ops)))
        (set-at-location! num-ops))

    ;; This is within safe Rosette so works fine
    (set-vec-map-num-operations! map (+ num-ops 1))))

(define (map-keys map)
  ;; Use the same trick as in map-set!
  (let* ([num-ops (vec-map-num-operations map)]
         [keys (vec-map-keys map)]
         [capacity (vector-length keys)]
         [max-capacity
          (if (union? capacity)
              (apply max (map second (union-contents capacity)))
              capacity)])

    ;; We deal with duplicates at the end
    (define (concrete-map-keys keys size)
      (for/list ([i size])
        (vector-ref keys i)))

    (define result
      (if (term? num-ops)
          (my-for/or ([possible-num-ops (+ max-capacity 1)])
            (and (= num-ops possible-num-ops)
                 (concrete-map-keys keys possible-num-ops)))
          (concrete-map-keys keys num-ops)))

    (remove-duplicates result)))

(define (map-values map)
  (for/all ([keys (map-keys map)])
    (for/list ([key keys])
      (map-ref map key))))
