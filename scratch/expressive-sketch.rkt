;; An example file that shows how to design a customizable grammar and
;; perform synthesis with it. It is better than define-synthax because
;; it is procedure-based, which makes it more efficient and more
;; flexible. In particular, you can have different kinds of terminals,
;; which are handled differently. You can also have different stopping
;; criteria (for example, a number that sets the number of statements,
;; and a number that sets the depth of each statement), whereas
;; define-synthax only allows you to use computations on a single
;; number to determine the stopping point.
;; normal-sketch.rkt shows how to achieve this with define-synthax.

#lang s-exp rosette

(require rosette/lib/angelic)

(provide (all-defined-out))

(current-bitwidth 16)

;;;;;;;;;;;;;
;; Lifting ;;
;;;;;;;;;;;;;

;; Lifting values enables synthesis where we can also recover the code
;; once synthesis is complete.
;; This only needs to be written once and can then be used with many
;; grammars (though case-lift may need to be extended, particularly
;; for variables).
(define-syntax (lift stx)
  (syntax-case stx ()
    [(lift thing ...)
     (syntax/loc stx
       (list (case-lift thing 'thing) ...))]))

;; Returns a lifted object.
(define (case-lift thing quoted-thing)
  (cond [(number? quoted-thing)
         (lambda (message)
           (unless (or (equal? message 'value) (equal? message 'code))
             (error (format "Unknown message ~a~%" message)))
           quoted-thing)]
        [(and (procedure? thing)
              (symbol? quoted-thing))
         (lambda args
           (lambda (message)
             (cond [(equal? message 'value)
                    (apply thing (map (lambda (x) (x 'value))
                                      args))]
                   [(equal? message 'code)
                    `(,quoted-thing ,@(map (lambda (x) (x 'code))
                                           args))]
                   [else
                    (error (format "Unknown message ~a~%" message))])))]
        [else
         (error (format "Cannot lift ~a which has value ~a~%"
                        quoted-thing thing))]))


;;;;;;;;;;;;;
;; Grammar ;;
;;;;;;;;;;;;;

;; A simple grammar where you provide a list of binary operators and a
;; list of operands, and it constructs all possible expressions up to
;; a certain depth.
;; In particular, it knows the distinction between operators and
;; operands, and will never put them in the wrong place (i.e. it only
;; creates expressions like (+ 5 7), not (5 5 5)). This is hard to
;; achieve in define-synthax, unless you hardcode the operators ahead
;; of time.
(define (my-grammar operators operands num)
  (if (= num 0)
      (apply choose* operands)
      (choose* (apply choose* operands)
               ((apply choose* operators)
                (my-grammar operators operands (- num 1))
                (my-grammar operators operands (- num 1))))))

;; An example use of the grammar for synthesis -- how to combine 5 and
;; 7 to produce an arbitrary number.
(define (expr n depth)
  (define operators (lift + - *))
  (define operands (lift 7 5))
  (define lifted-sketch (my-grammar operators operands depth))
  (define synth
    (synthesize
     #:forall '()
     #:guarantee (assert (= (lifted-sketch 'value) n))))
  (evaluate (lifted-sketch 'code) synth))

;; 2932 requires an expression of depth 4
;; cpu time: 457 real time: 1057 gc time: 69
;; So, about 0.457s to generate the formula, and 0.6s to solve it, I think
;; If you run "time racket expressive-sketch.rkt", then 1.6s total
(time (expr 2932 4))
