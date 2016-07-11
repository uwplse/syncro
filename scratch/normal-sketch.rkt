;; Companion file to expressive-sketch.rkt. It shows how to do the
;; same sort of sketch using define-synthax.

#lang s-exp rosette

(require rosette/lib/synthax)

(provide (all-defined-out))

(current-bitwidth 16)

;;;;;;;;;;;;;
;; Synthax ;;
;;;;;;;;;;;;;

;; Note that the grammar defined below does not distinguish between
;; operators and operands. In particular, it generates expressions
;; like (7 5 5) which are clearly illegal. There isn't an easy way to
;; avoid this, except by hardcoding the operators in the grammar. You
;; could also try generating a grammar at runtime where operators are
;; hardcoded, but I had trouble with that.
(define-synthax (expr-synthax terminal ... num)
  #:base (choose terminal ...)
  #:else (choose (choose terminal ...)
                 ((choose terminal ...)
                  (expr-synthax terminal ... (- num 1))
                  (expr-synthax terminal ... (- num 1)))))
(define (sketch a b c d e)
  (expr-synthax a b c d e 4))
(define (synthax-expr n)
  (define synth
    (synthesize
     #:forall '()
     #:guarantee (assert (= (sketch + - * 7 5) n))))
  (evaluate (sketch (lambda args `(+ ,@args))
                    (lambda args `(- ,@args))
                    (lambda args `(* ,@args))
                    7
                    5)
            synth))

;; cpu time: 180 real time: 531 gc time: 32
;; However, a lot of the work of creating the grammar happens at compile time.
;; If you run "time racket expressive-sketch.rkt", then 1.7s total
(time (synthax-expr 2932))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Grammar Generation ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; One attempt to fix the problems with define-synthax is to generate
;; the grammar at runtime. Here is an attempt at that, though it does
;; not work. (Make sure to require rosette-namespace.rkt to run this
;; code.)

#|
(define (fast-expr n terminal-hash)
  (define rosette-code-list
    `(
      (define-synthax (expr-synthax num)
        #:base (choose ,@(hash-ref terminal-hash 'operand))
        #:else (choose ,@(hash-ref terminal-hash 'operand)
                       ((choose ,@(hash-ref terminal-hash 'operator))
                        (expr-synthax (- num 1))
                        (expr-synthax (- num 1)))))

      (define (sketch)
        (display (format "Running sketch~%"))
        (expr-synthax 2))
      (sketch)

      (define synth
        (synthesize
         #:forall '()
         #:guarantee (assert (= (sketch) ,n))))

      (syntax->datum (car (generate-forms synth)))))
  (pretty-print rosette-code-list)
  (for ([code rosette-code-list])
    (eval code rosette-ns)))

(define terminal-hash #hash((operator . (+ - *)) (operand . (7 5))))
(pretty-print (fast-expr 9 terminal-hash))
|#
