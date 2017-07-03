#lang racket

;; Reimplementation of synthesize from Rosette with support for
;; printing the results from the guesser and checker.

(require
 (only-in rosette/base/core/reflect symbolics)
 (only-in rosette/query/core eval/asserts)
 rosette/query/eval rosette/query/finitize
 (only-in rosette/base/core/term constant? term-type get-type term-cache clear-terms! term<? solvable-default)
 (only-in rosette/base/core/equality @equal?)
 (only-in rosette/base/core/bool ! || && => with-asserts-only @boolean?)
 (only-in rosette/base/core/function fv)
 (only-in rosette/base/core/real @integer? @real?)
 (only-in rosette/base/core/bitvector bv bitvector?)
 rosette/solver/solver
 (only-in rosette/solver/solution model core sat unsat sat? unsat?)
 (only-in rosette/solver/smt/z3 z3))

(provide synthesize-with-printers)

;; Copied from rosette/query/form.rkt, with modifications for printing
(define-syntax synthesize-with-printers
  (syntax-rules (synthesize-with-printers)
    [(_ #:forall inputs #:assume pre #:guarantee post
        #:printers [print-guess print-cex])
     (∃∀-solve (symbolics inputs) 
               (eval/asserts (thunk pre)) 
               (eval/asserts (thunk post))
               #:print-guess print-guess
               #:print-cex print-cex)]    
    [(_ #:forall inputs #:guarantee post
        #:printers [print-guess print-cex])
     (synthesize-with-printers #:forall inputs #:assume #t #:guarantee post
                               #:printers [print-guess print-cex])]))

;; Copied from rosette/query/core.rkt, with modifications for printing
(define (∃∀-solve inputs assumes asserts #:print-guess [print-guess (const #t)] #:print-cex [print-cex (const #t)] #:solver [solver z3] #:bitwidth [bw (current-bitwidth)])
  (parameterize ([current-custodian (make-custodian)]
                 [current-subprocess-custodian-mode 'kill]
                 [term-cache (hash-copy (term-cache))])
    (with-handlers ([exn? (lambda (e) (custodian-shutdown-all (current-custodian)) (raise e))])
      (begin0 
        (cond 
          [bw
           (define fmap (finitize (append inputs assumes asserts) bw))
           (define fsol (cegis (for/list ([i inputs])  (hash-ref fmap i))
                               (for/list ([φ assumes]) (hash-ref fmap φ))
                               (for/list ([φ asserts]) (hash-ref fmap φ))
                               (solver) (solver)
                               print-guess print-cex fmap))
           (unfinitize fsol fmap)]
          [else 
           (cegis inputs assumes asserts (solver) (solver) print-guess print-cex)])
        (custodian-shutdown-all (current-custodian))))))
         

;; Copied from rosette/query/core.rkt, with modifications for printing
(define (cegis inputs assumes asserts guesser checker print-guess print-cex [fmap #f])
  
  (define φ   (append assumes asserts))
  
  (define ¬φ `(,@assumes ,(apply || (map ! asserts))))
   
  (define trial 0)
  
  (define (guess sol)
    (solver-assert guesser (evaluate φ sol))
    (match (solver-check guesser)
      [(model m) (sat (for/hash ([(c v) m] #:unless (member c inputs)) (values c v)))]
      [other other]))
  
  (define (check sol)
    (solver-clear checker)
    (solver-assert checker (evaluate ¬φ sol))
    (match (solver-check checker)
      [(? sat? m) (sat (for/hash ([i inputs])
                         (values i (let ([v (m i)])
                                     (if (eq? v i)
                                         (solvable-default (term-type i))
                                         v)))))]
      [other other]))
    
  (let loop ([candidate (begin0 (guess (sat)) (solver-clear guesser))])
    (cond 
      [(unsat? candidate) candidate]
      [else
        (print-guess (if fmap (unfinitize candidate fmap) candidate))
        (let ([cex (check candidate)])
          (cond 
            [(unsat? cex) candidate]
            [else (print-cex (if fmap (unfinitize cex fmap) cex))
                  (set! trial (add1 trial))
                  (loop (guess cex))]))])))
