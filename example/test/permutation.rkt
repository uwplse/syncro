#lang incremental

(define int (Integer-type))
(define-symbolic LEN #:type int #:configs [1 5])

(define-incremental permutation #:type (Vector-type LEN int)
  #:invariant (equal? (sort (vector->list permutation) <) (range LEN))
  #:initialize (build-vector LEN identity)
  #:updates
  [(define (swap! [i int] [j int])
     (let ([tmp (vector-ref permutation i)])
       (vector-set! permutation i (vector-ref permutation j))
       (vector-set! permutation j tmp)))
   (define (swap-no-tmp! [i int] [j int])
     (vector-set! permutation i
                  (+ (vector-ref permutation i) (vector-ref permutation j)))
     (vector-set! permutation j
                  (- (vector-ref permutation i) (vector-ref permutation j)))
     (vector-set! permutation i
                  (- (vector-ref permutation i) (vector-ref permutation j))))])

(define-incremental inverse-permutation #:type (Vector-type LEN int)
  #:value
  (let ([result (make-vector LEN 0)])
    (for ([i LEN])
      (vector-set! result (vector-ref permutation i) i))
    result)
  #:depends (permutation))

;; Expect
;; (swap! i j)
;; (vector-set! inverse-permutation (vector-ref permutation i) i)
;; (vector-set! inverse-permutation (vector-ref permutation j) j)

(algorithm
 (define (compose-perm x y)
   (build-vector LEN (lambda (i) (vector-ref y (vector-ref x i)))))
 (displayln inverse-permutation) ;; expect #(0 1 2 3 4)
 (swap! 1 2)
 (swap! 2 3)
 (swap! 0 4)
 (displayln permutation) ;; expect #(4 2 3 1 0)
 (displayln inverse-permutation) ;; expect #(4 3 1 2 0)
 ;; Composing inverses gives identity #(0 1 2 3 4)
 (displayln (compose-perm permutation inverse-permutation))
 (displayln (compose-perm inverse-permutation permutation)))
