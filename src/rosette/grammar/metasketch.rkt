#lang rosette

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
     ;; Computes the number of nodes in the program
     (fold-lifted program (const 1) +))
   
   (define (sketches self [c +inf.0])
     (set (make-sketch self '(2 3 1)))
     #;(list->set
      (for*/list ([i 4] [j 4])
        (make-sketch self (list (+ i 1) (+ j 1) 1)))))

   (define (get-sketch self idx)
     (make-sketch self idx))])

(define (make-sketch ms index)
  (grammar-sketch ms index #f #f))

(struct grammar-sketch (meta index [program #:mutable] [postconditions #:mutable])
  #:methods gen:sketch
  [(define (metasketch self)
     (grammar-sketch-meta self))

   (define (index self)
     (grammar-sketch-index self))
   
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
    (define ms (grammar-sketch-meta sketch))
    (match* [(grammar-sketch-meta sketch) (grammar-sketch-index sketch)]
      [((grammar-ms terminal-info _ _ _ _ options)
        `(,num-stmts ,expr-depth ,guard-depth))

       (when (hash-ref options 'verbose?)
         (printf "Creating symbolic program: (grammar ~a ~a ~a)~%"
                 num-stmts expr-depth guard-depth))
       
       (set-grammar-sketch-program!
        sketch
        (grammar terminal-info num-stmts expr-depth
                 #:num-temps 0 #:guard-depth guard-depth
                 #:version (hash-ref options 'grammar-version)
                 #:choice-version (hash-ref options 'grammar-choice)))]))
  
  (grammar-sketch-program sketch))

(define (get-postconditions sketch)
  
  (when (false? (grammar-sketch-postconditions sketch))
    ;; Reset to the initial state
    ((grammar-ms-reset-fn (grammar-sketch-meta sketch)))
    ;; Run the symbolic program
    (eval-lifted (get-program sketch))
    (set-grammar-sketch-postconditions!
     sketch
     ((grammar-ms-postconditions-fn (grammar-sketch-meta sketch)))))

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
  
  

