#lang s-exp rosette

(provide make-enum-set make-symbolic-enum-set
         enum-set-add! enum-set-remove! enum-set-contains? enum-set-size
         enum-set-union for-enum-set)

(define (make-enum-set num-things)
  (make-vector num-things #f))

(define (make-symbolic-enum-set num-things)
  (build-vector num-things (lambda (i) (define-symbolic* choice boolean?) choice)))

(define (enum-set-contains? set elem)
  (vector-ref set elem))

(define (enum-set-add! set elem)
  (assert (not (enum-set-contains? set elem)))
  (vector-set! set elem #t))

(define (enum-set-remove! set elem)
  (assert (enum-set-contains? set elem))
  (vector-set! set elem #f))

(define (enum-set-size set)
  (define result 0)
  (for-enum-set ([x set])
    (set! result (+ result 1)))
  result)

(define (enum-set-union x y . others)
  (define result (make-enum-set (vector-length x)))
  (define all (cons x (cons y others)))
  (for ([elem (vector-length x)])
    (when (ormap (lambda (x) (enum-set-contains? x elem)) all)
      (enum-set-add! result elem)))
  result)

(define-syntax-rule (for-enum-set ([var set-expr]) expr ...)
  (begin
    (define set set-expr)
    (for ([var (vector-length set)])
      (when (enum-set-contains? set var)
        expr ...))))