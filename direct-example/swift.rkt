#lang s-exp rosette

(require "enum-set.rkt" "util.rkt")

(current-bitwidth 8)

;;;;;;;;;;;;;;;
;; Constants ;;
;;;;;;;;;;;;;;;

(define NUM_NODES 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compute from scratch ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compute-parents-from-children)
  (define parents
    (build-vector NUM_NODES
                  (lambda (i) (make-enum-set NUM_NODES))))
  (for ([parent NUM_NODES])
    (for-enum-set ([child (vector-ref children parent)])
      (add-in-vector! parents child parent)))
  parents)

(define (compute-num)
  (define num (build-vector NUM_NODES (lambda (i) (make-vector NUM_NODES 0))))
  (for ([node NUM_NODES])
    (for ([n NUM_NODES])
      (for-enum-set ([m (vector-ref children node)])
        (when (enum-set-contains? (vector-ref children n) m)
          (matrix-increment! num node n)))))
  num)

(define (compute-tmp)
  (define tmp
    (build-vector NUM_NODES
                  (lambda (i) (make-enum-set NUM_NODES))))
  (for ([node NUM_NODES])
    (for ([n NUM_NODES])
      (when (and (not (equal? node n))
                 (> (matrix-get num node n) 0))
        (add-in-vector! tmp node n))))
  tmp)

(define (compute-mb)
  (define mb
    (build-vector NUM_NODES
                  (lambda (i) (make-enum-set NUM_NODES))))
  (for ([node NUM_NODES])
    (vector-set! mb node
                 (enum-set-union (vector-ref children node)
                                 (vector-ref parents node)
                                 (vector-ref tmp node))))
  mb)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Incremental Function ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-child child node)
  (add-in-vector! children node child)

  (matrix-increment! num node node)
  (for-enum-set ([n (vector-ref parents child)])
    (matrix-increment! num n node)
    (matrix-increment! num node n)
    (when (= (matrix-get num n node) 1)
      (add-in-vector! tmp node n)
      (add-in-vector! tmp n node)
      (when (not (enum-set-contains? (vector-ref mb node) n))
        (add-in-vector! mb node n)
        (add-in-vector! mb n node))))

  (add-in-vector! parents child node)
  (when (not (enum-set-contains? (vector-ref mb child) node))
    (add-in-vector! mb child node)
    (add-in-vector! mb node child)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Check for validity ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define (check)
  (assert (equal? parents (compute-parents-from-children)))
  (assert (equal? num (compute-num)))
  (assert (equal? tmp (compute-tmp)))
  (assert (equal? mb (compute-mb))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup and Incrementalization ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define children
  (build-vector NUM_NODES
                (lambda (i)
                  (make-symbolic-enum-set NUM_NODES))))

(define parents (compute-parents-from-children))

(define num (compute-num))
(define tmp (compute-tmp))
(define mb (compute-mb))

;(check)

(define-symbolic child-sym number?)
(assert (>= child-sym 0))
(assert (< child-sym NUM_NODES))
(define-symbolic parent-sym number?)
(assert (>= parent-sym 0))
(assert (< parent-sym NUM_NODES))
(assert (not (equal? child-sym parent-sym)))
(assert (not (enum-set-contains? (vector-ref children parent-sym) child-sym)))

(define (go)
  (verify (begin (add-child child-sym parent-sym) (check))))

; (time
;   (with-handlers ([exn:fail? (lambda (ex) "Prevent the exception")])
;     (go)))
; With NUM_NODES = 10:
; cpu time: 119324 real time: 675482 gc time: 13666
; With NUM_NODES = 5:
; cpu time: 1202 real time: 10351 gc time: 125