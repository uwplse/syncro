#lang racket

(provide (all-defined-out))

(define (symbol-append . args)
  (string->symbol (apply symbol-append (map symbol->string args))))

;; TODO: Turn these into classes, and then allow for type-directed code generation (for example, the "update-X" functions depend on the type)
(struct type () #:transparent)
(struct Integer-type type () #:transparent)
(struct Enum-type type (num-items) #:transparent)
(struct Vector-type type (input-type output-type) #:transparent)

(struct id-vector (id type children parents fn update-fn) #:transparent #:mutable)

(define id-table (make-hash))

(define-syntax-rule (define-mutable id update-id type expr ...)
  ;; TODO: type is recomputed twice here, but don't want to use a let because then update-id would not be global
  (begin
    ; (assert (not (hash-contains dependency-table 'id)))
    (define (update-id . args)
      (apply base-update id type args)
      (for ([dependency (id-vector-children (hash-ref id-table 'id))])
        ((id-vector-update-fn (hash-ref id-table dependency)))))

    (hash-set! id-table 'id
               (id-vector 'id type '() '()
                          (lambda () expr ...)
                          update-id))
    (define id (begin expr ...))
    (void)))

(define-syntax-rule (define-incremental id update-id type (dependency ...) expr ...)
  (begin
    ; (assert (not (hash-contains dependency-table 'id)))
    (hash-set! id-table 'id
               (id-vector 'id type '() '(dependency ...)
                          (lambda () expr ...)
                          (lambda () expr ...)))
    (begin
      (let ([dep (hash-ref id-table 'dependency)])
        (set-id-vector-children! dep (cons 'id (id-vector-children dep))))
      ...)
    (define id (begin expr ...))
    (void)))

(define (base-update thing type . args)
  (apply vector-set! thing args))
