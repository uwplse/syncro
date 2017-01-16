#lang rosette

(require "grammar.rkt" "rosette-util.rkt" "../util.rkt"
         "../../../../synapse/opsyn/engine/search.rkt"
         "../../../../synapse/opsyn/engine/metasketch.rkt")

(provide search (rename-out [make-grammar-ms grammar-metasketch]))

(define (make-grammar-ms info inputs postcondition-fn reset-fn)
  (grammar-ms info (map input-val inputs)
                      (foldl append '()
                             (map input-preconditions inputs))
                      postcondition-fn
                      reset-fn))

(struct grammar-ms (terminal-info inputs preconditions postcondition-fn reset-fn)
  #:methods gen:metasketch
  [(define (inputs self)
     (map input-val (grammar-ms-inputs self)))
   
   (define (structure self sketch)
     ;; TODO: Add structure constraints to sketches
     '())
   
   (define (min-bitwidth self sketch)
     ;; TODO: Is this the right thing?
     1)
   
   (define (cost self program)
     ;; Computes the number of nodes in the program
     (fold-lifted (const 1) + program))
   
   (define (sketches self [c +inf.0])
     (list->set
      (for*/list ([i 4] [j 4])
        (make-sketch self (list i j 1)))))])

(define (make-sketch ms index)
  (grammar-sketch ms index #f #f))

(struct grammar-sketch (meta index [program #:mutable] [postconditions #:mutable])
  #:methods gen:sketch
  [(define (metasketch self)
     (grammar-sketch-meta self))
   
   (define (programs self [sol (sat)])
      (if (zero? (dict-count (model sol)))
          (get-program self)
          (evaluate (get-program self) sol)))
   
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
    (match-define (list num-stmts expr-depth guard-depth)
      (grammar-sketch-index sketch))
    (define terminal-info
      (grammar-ms-terminal-info (grammar-sketch-meta sketch)))
    (printf "EXPENSIVE: Calling (grammar ~a ~a ~a)~%" num-stmts
            expr-depth guard-depth)
    (set-grammar-sketch-program!
     sketch
     (grammar terminal-info num-stmts expr-depth
              #:num-temps 0 #:guard-depth guard-depth
              #:version 'caching #:choice-version 'basic)))
  
  (grammar-sketch-program sketch))

(define (get-postconditions sketch)
  
  (when (false? (grammar-sketch-postconditions sketch))
    ;; Reset to the initial state
    ((grammar-ms-reset-fn (grammar-sketch-meta sketch)))
    ;; Run the symbolic program
    (eval-lifted (get-program sketch))
    (set-grammar-sketch-postconditions!
     sketch
     (list ((grammar-ms-postcondition-fn (grammar-sketch-meta sketch))))))

  (grammar-sketch-postconditions sketch))

;; (struct space (c) 
;;   #:transparent
;;   #:guard (lambda (c name) (max c 0))
;;   #:property prop:sequence
;;   (lambda (self) (in-set self))
;;   #:methods gen:set
;;   [(define (set-count self) 
;;      (match self
;;        [(space 0) 0]
;;        [(space c) (+ 1 (/ (* (- c 1) c) 2))]))
   
;;    (define (set-member? self idx)
;;      (match-define (list i b) idx)
;;      (and
;;       (< i (space-c self))
;;       (or (and (= i 0) (= b 0))
;;           (< b i))))
   
;;    (define (in-set self)
;;      (in-generator
;;       (match self 
;;         [(space c)
;;          (when (> c 0)
;;            (yield '(0 0)))
;;          (for*([i (in-range 0 c)][b i]) 
;;            (yield (list i b)))])))])
  
  

