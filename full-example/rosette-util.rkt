;; Lifting values enables synthesis where we can also recover the code
;; once synthesis is complete.
#lang s-exp rosette

(provide define-lifted lift lifted-value? begin^
         eval-lifted lifted-code)

;; Lifting values enables synthesis where we can also recover the code
;; once synthesis is complete.

;; Since Rosette doesn't support objects, we'll use structs and
;; generic functions on those structs.
(struct lifted-value () #:transparent)
(struct lifted-variable lifted-value (val var) #:transparent)
(struct lifted-procedure lifted-value (proc proc-name args) #:transparent)
(struct lifted-begin lifted-value (args) #:transparent)

(define (lift value var-name)
  (cond [(and (procedure? value) (symbol? var-name))
         (lambda arguments
           (lifted-procedure value var-name arguments))]
        [(symbol? var-name)
         (lifted-variable value var-name)]
        [else
         (error (format "Cannot lift ~a which has value ~a~%"
                        var-name value))]))

(define (begin^ . args)
  (lifted-begin args))

(define (maybe-eval-lifted x)
  (if (lifted-value? x)
      (eval-lifted x)
      x))

(define (eval-lifted x)
  (cond [(lifted-variable? x)
         (lifted-variable-val x)]
        [(lifted-procedure? x)
         (apply (lifted-procedure-proc x)
                (map maybe-eval-lifted (lifted-procedure-args x)))]
        [(lifted-begin? x)
         (for ([arg (lifted-begin-args x)])
           (maybe-eval-lifted arg))]
        [(lifted-value? x)
         (error (format "Internal error: Unhandled lifted value ~a in eval-lifted~%" x))]
        [else
         (error (format "Argument to eval-lifted is not a lifted value: ~a~%" x))]))

(define (maybe-lifted-code x)
  (if (lifted-value? x)
      (lifted-code x)
      x))

(define (lifted-code x)
  (cond [(lifted-variable? x)
         (lifted-variable-var x)]
        [(lifted-procedure? x)
         `(,(lifted-procedure-proc-name x)
           ,@(map maybe-lifted-code (lifted-procedure-args x)))]
        [(lifted-begin? x)
         `(begin ,@(map maybe-lifted-code (lifted-begin-args x)))]
        [(lifted-value? x)
         (error (format "Internal error: Unhandled lifted value ~a in eval-lifted~%" x))]
        [else
         (error (format "Argument to eval-lifted is not a lifted value: ~a~%" x))]))


;; (define Code-Creator%
;;   (class object%
;;     (super-new)

;;     (define/public (evaluate)
;;       (error (format "~a does not implement evaluate" this)))

;;     (define/public (code)
;;       (error (format "~a does not implement code" this)))))

;; (define Code-Creator-for-Variable%
;;   (class Code-Creator%
;;     (super-new)
;;     (init-field var val)
;;     (define/override (evaluate) val)
;;     (define/override (code) var)))

;; (define Code-Creator-for-Procedure%
;;   (class Code-Creator%
;;     (super-new)
;;     (init-field proc proc-name args)

;;     (define/override (evaluate)
;;       (apply proc
;;              (map (lambda (arg)
;;                     (if (is-a? arg Code-Creator%)
;;                         (send arg evaluate)
;;                         arg))
;;                   args)))

;;     (define/override (code)
;;       `(,proc-name ,@(map (lambda (arg)
;;                             (if (is-a? arg Code-Creator%)
;;                                 (send arg code)
;;                                 arg))
;;                           args)))))

;; (define Code-Creator-for-Begin%
;;   (class Code-Creator%
;;     (super-new)
;;     (init-field args)

;;     (define/override (evaluate)
;;       (for ([arg args])
;;         (if (is-a? arg Code-Creator%)
;;             (send arg evaluate)
;;             arg)))
    
;;     (define/override (code)
;;       `(begin ,@(map (lambda (arg)
;;                        (if (is-a? arg Code-Creator%)
;;                            (send arg code)
;;                            arg))
;;                      args)))))

;; (define (is-lifted? thing)
;;   (is-a? thing Code-Creator%))

(define-syntax (define-lifted stx)
  (syntax-case stx ()
    [(define-lifted [thing new-name] ...)
     (syntax/loc stx
       (begin (define new-name (lift thing 'thing))
              ...))]))

;; Returns a lifted object.
;; (define (lift value var-name)
;;   (cond [(and (procedure? value)
;;               (symbol? var-name))
;;          (lambda arguments
;;            (new Code-Creator-for-Procedure%
;;                 [proc value]
;;                 [proc-name var-name]
;;                 [args arguments]))]
;;         [(symbol? var-name)
;;          (new Code-Creator-for-Variable%
;;               [var var-name]
;;               [val value])]
;;         [else
;;          (error (format "Cannot lift ~a which has value ~a~%"
;;                         var-name value))]))

;; (define (begin^ . arguments)
;;   (new Code-Creator-for-Begin% [args arguments]))
