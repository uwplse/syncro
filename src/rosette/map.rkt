#lang rosette

(require "util.rkt")

(provide make-map make-symbolic-map
         map-has-key? map-ref map-set!
         map-keys map-values)

(struct vec-map ([size #:mutable] keys values) #:transparent)

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

  (define-symbolic* size integer?)
  (when varset
    (define assertions (list (>= size 0) (<= size capacity)))
    (set-add! varset (make-input size assertions)))

  ;; TODO: Would it be bad if we eliminate the (< i size) guard? It
  ;; might make the generated formulas simpler.
  (vec-map size
           (for/vector #:length capacity ([i capacity])
             (and (< i size) (input-fn)))
           (for/vector #:length capacity ([i capacity])
             (and (< i size) (output-fn)))))

(define (map-has-key? map key)
  (for*/all ([keys (vec-map-keys map)]
             [capacity (vector-length keys)])
    (my-for/or ([i capacity])
      (and (< i (vec-map-size map))
           (equal? key (vector-ref keys i))))))

(define (map-ref map key)
  (for*/all ([keys (vec-map-keys map)]
             [capacity (vector-length keys)])
    (let* ([vals (vec-map-values map)])
      (my-for/or ([i capacity])
        ;; Iterate backwards so you hit later transactions first
        (let ([index (- capacity i 1)])
          (and (< index (vec-map-size map))
               (equal? key (vector-ref keys index))
               (vector-ref vals index)))))))

(define (map-set! map key val)
  ;; This is tricky. You are not supposed to use for/all to do
  ;; imperative stuff. Need to think carefully about how this should
  ;; be implemented.
  ;; Make sure that when modifying the vector you add a guard that
  ;; ensures that it retains the original value in cases where the
  ;; guard is not true.
  ;; TODO: Think about whether this makes sense taking path conditions
  ;; into account.
  (for/all ([size (vec-map-size map)])
    (let* ([keys (vec-map-keys map)]
           [vals (vec-map-values map)]
           [condition (= size (vec-map-size map))])
      (unless (< size (vector-length keys))
        (maybe-internal-error "Map is full!"))
      
      (vector-set! keys size (if condition key (vector-ref keys size)))
      (vector-set! vals size (if condition val (vector-ref vals size)))))

  ;; This is within safe Rosette so works fine
  (set-vec-map-size! map (+ 1 (vec-map-size map))))

(define (map-keys map)
  (define vec (vec-map-keys map))

  (define (concrete-map-keys map size)
    (for/list ([i size])
      (vector-ref vec i)))

  (define result
    (for/all ([size (vec-map-size map)])
      (if (term? size)
          ;; Symbolic constant, need to finitize ourselves
          ;; TODO: Can we do a for/all on the vector itself instead?
          ;; This could improve symbolic evaluation, since
          ;; concrete-map-keys could work on the concrete vector,
          ;; but would only work if after a for/all the vector is
          ;; guaranteed to have a concrete length.
          (for/all ([len (vector-length vec)])
            (my-for/or ([concrete-size len])
              (and (= size concrete-size)
                   (concrete-map-keys map concrete-size))))

          (concrete-map-keys map size))))

  (remove-duplicates result))

(define (map-values map)
  (for/all ([keys (map-keys map)])
    (for/list ([key keys])
      (map-ref map key))))
