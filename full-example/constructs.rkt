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


(define-for-syntax (make-id template . ids)
  (string->symbol (apply format template (map syntax->datum ids))))

(define-syntax (define-mutable stx)
  (syntax-case stx ()
    [(_ id type expr ...)
     (with-syntax ([update-id
                    (datum->syntax stx (make-id "update-~a!" #'id))])
       (syntax/loc stx
         ;; TODO: type is recomputed twice here, but don't want to use a let because then update-id would not be global
         (begin
           (when (hash-has-key? id-table 'id)
             (error (string-append "Symbol has already been used: " (symbol->string 'id))))
           (define (update-id . args)
             (apply base-update id type args)
             (for ([dep (id-vector-children (hash-ref id-table 'id))])
               ((id-vector-update-fn (hash-ref id-table dep)))))
           
           (hash-set! id-table 'id
                      (id-vector 'id type '() '()
                                 (lambda () expr ...)
                                 update-id))
           (define id (begin expr ...))
           (void))))]))

(define-syntax (define-incremental stx)
  (syntax-case stx ()
    [(_ id type (dependency ...) expr ...)
     (with-syntax ([update-id
                    (datum->syntax stx (make-id "update-~a!" #'id))])
       (syntax/loc stx
         (begin
           (when (hash-has-key? id-table 'id)
             (error (string-append "Symbol has already been used: " (symbol->string 'id))))
           (hash-set! id-table 'id
                      (id-vector 'id type '() '(dependency ...)
                                 (lambda () expr ...)
                                 (lambda ()
                                   (set! id (begin expr ...))
                                   (for ([dep (id-vector-children (hash-ref id-table 'id))])
                                     ((id-vector-update-fn (hash-ref id-table dep)))))))
                            
           (begin
             (let ([dep (hash-ref id-table 'dependency)])
               (set-id-vector-children! dep (cons 'id (id-vector-children dep))))
             ...)
           (define id (begin expr ...))
           (void))))]))

(define (base-update thing type . args)
  (apply vector-set! thing args))
