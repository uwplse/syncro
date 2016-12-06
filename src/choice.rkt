#lang rosette

(require (only-in rosette/lib/angelic [choose* rosette-choose*]))

(provide make-chooser choose* choose*-fn)

(define-syntax (choose* stx)
  (syntax-case stx ()
    [(_ chooser default arg ...)
     (syntax/loc stx
       (begin
         (send chooser start-options)
         (let ([args '()])
           (begin (begin (send chooser begin-next-option)
                         (set! args (cons arg args)))
                  ...)
           (send chooser end-options)
           (apply choose*-fn chooser default (reverse args)))))]))

(define (choose*-fn chooser default . args)
  (send chooser choose* args default))

(define (make-chooser version)
  (cond [(equal? version 'basic)
         (new Identity-Chooser%)]
        [(equal? version 'sharing)
         (new Sharing-Chooser%)]
        [else
         (error (format "Unknown chooser type: ~a" version))]))


;; Calls here should never be symbolic, so we might as well implement
;; this with a class instead of as a struct
(define Identity-Chooser%
  (class object%
    (super-new)
    (field [num-vars 0])

    (define/public (get-num-vars) num-vars)

    (define/public (choose* args default)
      (if (null? args)
          default
          (begin (set! num-vars (+ num-vars -1 (length args)))
                 (apply rosette-choose* args))))

    (define/public (start-options) (void))
    (define/public (end-options) (void))
    (define/public (begin-next-option) (void))))


(define Sharing-Chooser%
  (class object%
    (super-new)

    ;; TODO: This is a really simple but inefficient implementation.
    ;; Better data structures could likely make all of the operations
    ;; amortized constant time. (Several are linear time right now.)
    ;; Invariants:
    ;; num-vars = (length vars)
    ;; val is a valid index into vars iff 0 <= val < num-vars
    ;; curr-ptr is either #f or a valid index into vars
    ;; curr-ptr is either #f or >= (car checkpoints)
    ;; checkpoints is sorted in descending order
    ;; All values v in checkpoints satisfy 0 <= v <= num-vars
    (field [vars '()] [num-vars 0] [curr-ptr #f]
           [checkpoints '()] [checkpoint->max-index (make-hash)])

    (define/public (get-num-vars) num-vars)

    (define/public (get-var)
      (if curr-ptr
          (begin (define result (list-ref vars curr-ptr))
                 (set! curr-ptr (+ 1 curr-ptr))
                 (when (>= curr-ptr num-vars)
                   (set! curr-ptr #f))
                 result)
          (begin (define-symbolic* x boolean?)
                 (set! num-vars (+ num-vars 1))
                 (set! vars (append vars (list x)))
                 x)))

    (define/public (choose* args default)
      (define (loop args)
        (if (null? (cdr args))
            (car args)
            (if (get-var)
                (car args)
                (loop (cdr args)))))
      
      (if (null? args)
          default
          (loop args)))

    (define/public (start-options)
      (set! checkpoints
            (cons (if curr-ptr curr-ptr num-vars) checkpoints))
      (begin-next-option))

    (define/public (end-options)
      (begin-next-option)
      (let ([new-position (hash-ref checkpoint->max-index (car checkpoints))])
        (set! curr-ptr (if (< new-position num-vars) new-position #f))
        (set! checkpoints (cdr checkpoints))))

    ;; Note: It is important that it does not make a difference if
    ;; this is called more than once at the beginning of each option.
    ;; start-options and end-options rely on this.
    (define/public (begin-next-option)
      (when (and curr-ptr (< curr-ptr (car checkpoints)))
        (error "Internal error: curr-ptr is before the first checkpoint"))
      (let* ([checkpoint (car checkpoints)]
             [curr-max (hash-ref checkpoint->max-index checkpoint -1)])
        (if curr-ptr
            (hash-set! checkpoint->max-index checkpoint (max curr-ptr curr-max))
            (hash-set! checkpoint->max-index checkpoint (max num-vars curr-max)))

        (if (>= checkpoint num-vars)
            (set! curr-ptr #f)
            (set! curr-ptr checkpoint))))))
