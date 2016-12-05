#lang s-exp rosette

(provide enum-make-set enum-make-symbolic-set
         enum-make-symbolic-set-with-tracking
         enum-set-add! enum-set-remove! enum-set-contains? enum-set-size
         enum-set-union for-enum-set)

(define (enum-make-set num-things)
  (make-vector num-things #f))

(define (enum-make-symbolic-set num-things)
  (build-vector num-things (lambda (i) (define-symbolic* choice boolean?) choice)))

(define (enum-make-symbolic-set-with-tracking num-things varset)
  (build-vector num-things
                (lambda (i)
                  (define-symbolic* choice boolean?)
                  (set-add! varset choice)
                  choice)))

(define (enum-set-contains? set elem)
  (vector-ref set elem))

(define (enum-set-add! set elem)
  (vector-set! set elem #t))

(define (enum-set-remove! set elem)
  (vector-set! set elem #f))

(define (enum-set-size set)
  (define result 0)
  (for-enum-set ([x set])
    (set! result (+ result 1)))
  result)

(define (enum-set-union x y . others)
  (define result (enum-make-set (vector-length x)))
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
