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
  (match version
    ['basic (make-basic-chooser)]
    ['sharing (new Sharing-Chooser%)]
    ['mock (make-mock-chooser)]
    [_ (error (format "Unknown chooser type: ~a" version))]))


;; Calls here should never be symbolic, so we might as well implement
;; this with a class instead of as a struct
(define Simple-Chooser%
  (class object%
    (super-new)
    (init-field choice-fn)
    (field [stats (new Stats-Counter%)])

    (define/public (print-stats)
      (printf "Used ~a boolean variables to encode a search space of log size ~a~%"
              (send stats get-num-vars)
              (send stats get-search-space)))

    (define/public (choose* args default)
      ;; Even if args is null, there is one choice (the default)
      (send stats record-choices (max 1 (length args)))
      (if (null? args)
          default
          (apply choice-fn args)))

    (define/public (start-options) (void))
    (define/public (end-options) (void))
    (define/public (begin-next-option) (void))))


(define (make-basic-chooser)
  (new Simple-Chooser% [choice-fn rosette-choose*]))

;; The Mock chooser always chooses the first option. It can be used to
;; test grammars -- by using the mock chooser, we prevent symbolic
;; variables from being introduced (except a few cases like integer
;; holes and boolean variables), which makes it possible to write
;; concrete tests that catch type errors and similar mistakes.
;; The number of vars reported by the Mock Chooser will be the same as
;; for the Basic Chooser.
(define (make-mock-chooser)
  (new Simple-Chooser% [choice-fn (lambda args (first args))]))


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
    (field [stats (new Stats-Counter%)] [num-vars 0]
           [vars '()] [curr-ptr #f]
           [checkpoints '()] [checkpoint->max-index '()])

    (define/public (print-stats)
      (printf "Used ~a boolean variables instead of the naive ~a variables to encode a search space of log size ~a~%"
              num-vars
              (send stats get-num-vars)
              (send stats get-search-space)))
    
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
      (send stats record-choices (max 1 (length args)))
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
      (set! checkpoint->max-index
            (cons (if curr-ptr curr-ptr num-vars) checkpoint->max-index)))

    (define/public (end-options)
      ;; Updates the max index if necessary
      (begin-next-option)
      (let ([new-position (car checkpoint->max-index)])
        (set! curr-ptr (if (< new-position num-vars) new-position #f))
        (set! checkpoint->max-index (cdr checkpoint->max-index))
        (set! checkpoints (cdr checkpoints))))

    ;; Note: It is important that it does not make a difference if
    ;; this is called more than once at the beginning of each option.
    ;; start-options and end-options rely on this.
    (define/public (begin-next-option)
      (when (and curr-ptr (< curr-ptr (car checkpoints)))
        (error "Internal error: curr-ptr is before the first checkpoint"))
      (let ([checkpoint (car checkpoints)]
            [curr-max (car checkpoint->max-index)])
        (set! checkpoint->max-index
              (cons (max (if curr-ptr curr-ptr num-vars) curr-max)
                    (cdr checkpoint->max-index)))

        (if (>= checkpoint num-vars)
            (set! curr-ptr #f)
            (set! curr-ptr checkpoint))))))


(define Stats-Counter%
  (class object%
    (super-new)
    (field [num-vars 0] [search-space 0])

    (define/public (record-choices num-choices)
      (set! num-vars (+ num-vars (- num-choices 1)))
      (set! search-space (+ search-space (log num-choices))))

    (define/public (get-num-vars) num-vars)
    (define/public (get-search-space) (/ search-space (log 2)))))
