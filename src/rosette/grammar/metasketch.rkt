#lang rosette

(require racket/generator)
(require "grammar.rkt" "language.rkt" "../util.rkt"
         "../../../../synapse/opsyn/engine/search.rkt"
         "../../../../synapse/opsyn/engine/metasketch.rkt")

(provide search (rename-out [make-grammar-ms grammar-metasketch]))

(define (make-grammar-ms info inputs postconditions-fn reset-fn options)
  (grammar-ms info (map input-val inputs)
                      (foldl append '()
                             (map input-preconditions inputs))
                      postconditions-fn
                      reset-fn
                      options))

(struct grammar-ms (terminal-info inputs preconditions postconditions-fn reset-fn options)
  #:methods gen:metasketch
  [(define (inputs self)
     (grammar-ms-inputs self))
   
   (define (structure self sketch)
     ;; TODO: Add structure constraints to sketches
     '())
   
   (define (min-bitwidth self sketch)
     ;; TODO: Is this the right thing?
     1)
   
   (define (cost self program)
     ;; Compute the number of nodes in the program
     #;(fold-lifted program (const 1) +)
     ;; Compute cost based solely on the sketch.
     (sum (first program)))
        
   
   (define (sketches self [c +inf.0])
     (sketch-space self c))

   (define (get-sketch self idx)
     (make-sketch self idx))])

(define (make-sketch ms index)
  (grammar-sketch ms index #f #f #f))

(struct grammar-sketch (meta index
                             [program #:mutable]
                             [grammar-assertions #:mutable]
                             [postconditions #:mutable])
  #:methods gen:sketch
  [(define (metasketch self)
     (grammar-sketch-meta self))

   (define (index self)
     (grammar-sketch-index self))

   ;; To enable the cost function to look at the sketch index, we'll
   ;; just have a program be a list of two elements -- the sketch that
   ;; generated it, and the actual symbolic program.
   (define (programs self [sol (sat)])
     (list (grammar-sketch-index self)
           (if (zero? (dict-count (model sol)))
               (get-program self)
               (evaluate (get-program self) sol))))
   
   (define (pre self)
     (grammar-ms-preconditions (grammar-sketch-meta self)))
   
   (define (post self P)
     (get-postconditions self))]
  
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (fprintf port "~a" (cons 'grammar (grammar-sketch-index self))))]
  
  #:methods gen:equal+hash
  [(define (equal-proc s1 s2 =?)
     (=? (grammar-sketch-index s1) (grammar-sketch-index s2)))
   (define (hash-proc s1 hash-code)
      (hash-code (grammar-sketch-index s1)))
   (define (hash2-proc s1 hash-code)
      (hash-code (grammar-sketch-index s1)))])

(define (get-program sketch)
  (when (false? (grammar-sketch-program sketch))
    (define ms (grammar-sketch-meta sketch))
    (match* [(grammar-sketch-meta sketch) (grammar-sketch-index sketch)]
      [((grammar-ms terminal-info _ _ _ _ options)
        `(,num-stmts ,expr-depth ,guard-depth))

       (when (hash-ref options 'verbose?)
         (printf "Creating symbolic program: (grammar ~a ~a ~a)~%"
                 num-stmts expr-depth guard-depth))

       ;; Generate the symbolic program, collecting any generated assertions
       (define-values (sym-prog assertions)
         (with-asserts
            (grammar terminal-info num-stmts expr-depth
                     #:num-temps 0 #:guard-depth guard-depth
                     #:version (hash-ref options 'grammar-version)
                     #:choice-version (hash-ref options 'grammar-choice))))
       (set-grammar-sketch-program! sketch sym-prog)
       (set-grammar-sketch-grammar-assertions! sketch assertions)]))
  
  (grammar-sketch-program sketch))

(define (get-postconditions sketch)
  
  (when (false? (grammar-sketch-postconditions sketch))
    (define ms (grammar-sketch-meta sketch))
    
    ;; Reset to the initial state
    ((grammar-ms-reset-fn ms))

    (define sym-prog (get-program sketch))
    (define-values (_ assertions)
      ;; If the program would just raise an error, don't run it and
      ;; just add the assertion #f to make it immediately UNSAT.
      (if (and (not (symbolic? sym-prog)) (lifted-error? sym-prog))
          (values #f (list #f))
          ;; Run the symbolic program, collecting assertions
          (with-asserts (eval-lifted (get-program sketch)))))

    ;; Calculate postconditions
    (define posts ((grammar-ms-postconditions-fn ms)))

    ;; Set postconditions, including generated assertions
    (set-grammar-sketch-postconditions!
     sketch
     (append assertions (grammar-sketch-grammar-assertions sketch) posts)))

  (grammar-sketch-postconditions sketch))


(struct sketch-space (ms cost)
  #:transparent
  #:property prop:sequence
  (lambda (self) (in-set self))
  #:methods gen:set
  [(define (set-count self)
     (define c (sketch-space-cost self))
     (+ 1 (for/sum ([guard 3])
            (triangle (- c guard 1)))))
   
   (define (set-member? self sketch)
     (and (grammar-sketch? sketch)
          (equal? (grammar-sketch-meta sketch) (sketch-space-ms self))
          (< (sum (grammar-sketch-index sketch)) (sketch-space-cost self))))
   
   (define (in-set self)
     (match self
       [(sketch-space ms max-cost)
        (in-generator
         (yield (make-sketch ms '(0 0 0)))
         (for* ([c (in-range 0 max-cost)]
                [guard (in-range 0 (min 3 (- c 1)))]
                [stmt (in-range 1 (- c guard))])
           (yield (make-sketch ms (list stmt (- c guard stmt) guard)))))]))])

(define (sum lst)
  (for/sum ([x lst]) x))

(define (triangle n)
  (if (<= n 0) 0 (/ (* n (+ n 1)) 2)))
