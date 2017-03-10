#lang racket

(require "dependency-graph.rkt"
         "namespace.rkt"
         "program.rkt"
         "../rosette/rosette-namespace.rkt"
         "../rosette/grammar/language.rkt"
         "../rosette/types.rkt"
         "../rosette/variable.rkt")

(provide perform-synthesis)

;; Various helpers that simply get information out of data structures.

(define (get-constant-info vars)
  (let ([typed-vars (filter variable-has-type? vars)])
    (list (map variable-symbol typed-vars)
          (map variable-type typed-vars))))

(define (get-node-info node)
  (let ([id (send node get-id)]
        [expr (send node get-fn-code)])
    (list node id (send node get-type) expr
          `(define ,id ,expr) (send node get-assumes-code))))

(define (get-symbolic-info node update-type set-id)
  (let ([update-args (send node get-update-arg-names update-type)])
    (append (list (send node get-symbolic-code set-id))
            (list update-args)
            (send node get-symbolic-update-code update-type update-args set-id)
            (send/apply node get-old-values-code update-type update-args))))

;; Transposes a list of lists.
;; eg. (transpose '((1 2 3) (4 5 6)) 3) gives '((1 4) (2 5) (3 6))
(define (transpose list-of-lists len)
  (unless (or (null? list-of-lists)
              (= len (length (car list-of-lists))))
    (error "Invalid arguments to transpose: ~a and ~a~%" list-of-lists len))
  (foldr (lambda (x y) (map cons x y))
         (build-list len (lambda (x) '()))
         list-of-lists))

;; Converts any (possibly nested) syntax objects to datums.
(define (datumify thing)
  (cond [(syntax? thing) (syntax->datum thing)]
        [(list? thing)
         (map datumify thing)]
        [else thing]))

(define (make-id template . ids)
  (string->symbol (apply format template ids)))

;; Creates the necessary Rosette code for synthesis, runs it, and
;; creates the relevant update functions.
;; prog: A program struct (see read-file.rkt)
(define (perform-synthesis prog options)
  (define graph (program-dependency-graph prog))
  
  ;; Relevant information about constants
  (match-define (list constant-terminal-ids constant-terminal-types)
    (get-constant-info (program-constants prog)))
  
  (define (get-id-info id)
    (get-node-info (get-node graph id)))
  
  ;; Relevant information about the input relation
  (match-define (list input-relation input-id input-type _ _ input-assumes)
    (get-id-info (car (get-ids graph))))

  (for/list ([update-name (send input-relation get-update-names)])
    
    ;; Relevant information for performing symbolic computation on the input
    ;; Note: It's important to call this only once per update
    ;; type so that the variables have consistent names.
    (match-define (list define-input update-args update-defns-code update-code update-arg-types define-overwritten-vals overwritten-vals overwritten-vals-types)
      (datumify
       (get-symbolic-info input-relation update-name 'inputs-set)))

    (define intermediate-ids '())
    (for ([output-id (cdr (get-ids graph))])
      (printf "Synthesizing update rule for ~a upon update ~a to ~a~%"
              output-id update-name input-id)
      
      ;; Relevant information about intermediate relations
      ;; Does a lot of recomputation, but not a bottleneck
      (match-define (list intermediate-relations _ intermediate-types _ define-intermediates _)
        (transpose (map get-id-info intermediate-ids) 6))

      ;; Relevant information about the output relation
      (define output-node (get-node graph output-id))
      (match-define (list output-relation _ output-type output-expr define-output _)
        (get-node-info output-node))

      (define (add-terminal-code var type #:mutable? [mutable? #f])
        ;; Note: Here we use (repr type) to get an expression that
        ;; evaluates to the type, but we could also store this
        ;; expression taken straight from the user program.
        `(send terminal-info make-and-add-terminal ',var ,var ,(repr type)
               #:mutable? ,mutable?))

      (define add-terminals
        (map add-terminal-code
             (append (list input-id) intermediate-ids
                     overwritten-vals update-args
                     constant-terminal-ids)
             (append (list input-type) intermediate-types
                     overwritten-vals-types update-arg-types
                     constant-terminal-types)))

      (define initialization-stmts
        `(;; Example: (define NUM_WORDS 12)
          ,@(program-initialization prog)
          (define inputs-set (mutable-set))
          ;; Example: (define word->topic (build-vector 12 ...))
          ;; The resulting data structure contains symbolic variables.
          ,define-input
          (define input-assertions ,input-assumes)
          ;; Example: (define num2helper (build-vector ...))
          ;; Taken straight from the user program, but operates on
          ;; symbolic variables.
          ;; This needs to be done here in case the output depends on
          ;; intermediate relations.
          ,@define-intermediates
          ;; Example: (define num2 (build-vector ...))
          ;; Basically the same as for intermediates.
          ,define-output))

      ;; update-stmts assumes that the output relation has been defined.
      ;; It also assumes that inputs-set has been defined.
      (define update-stmts
        `(;; Example:
          ;; (define-symbolic* index9079 integer?)
          ;; (assert (>= index9079 0))
          ;; (assert (< index9079 12))
          ;; (set-add! inputs-set index9079)
          ;; (define-symbolic* val9080 integer?)
          ;; (assert (>= val9080 0))
          ;; (assert (< val9080 3))
          ;; (set-add! inputs-set val9080)
          ,update-defns-code
          ;; Example:
          ;; (define old-value9082 (vector-ref word->topic index9079))
          ,define-overwritten-vals
          ;; Example: (vector-set! word->topic index9079 val9080)
          ,update-code))

      ;; Example: (set! num2helper (build-vector ...))
      ;; Taken straight from the user program, but operates on
      ;; symbolic variables.
      ;; Note that this must come after the update.
      ;; TODO: For now, we have to define intermediates before
      ;; the update in case the output relation depends on
      ;; them. So here, we should use a set!
      (define update-intermediate-stmts
        (map (lambda (code) `(set! ,@(cdr code))) define-intermediates))

      ;; Defines a reset function that when called will reset all data
      ;; structures to their state at the point where the function was defined.
      (define (add-reset-fn name)
        `((define init-state (clone (list ,input-id ,output-id ,@intermediate-ids)))
          (define (,name)
            (define clone-state (clone init-state))
            ,@(for/list ([id (cons input-id (cons output-id intermediate-ids))]
                         [index (in-naturals 0)])
                `(set! ,id (list-ref clone-state ,index))))))

      (define terminal-info-stmts
        ;; Create the grammar and sample a program
        `((define terminal-info (new Terminal-Info%))
          ,(add-terminal-code output-id output-type #:mutable? #t)
          ,@add-terminals))

      ;; Note: It is not (equal? ,output-id ,output-expr)
      ;; because when we run the lifted program, it modifies
      ;; the value *stored in the lifted program*, which is not
      ;; necessarily the same as the value in ,output-id.
      ;; Example: (assert (equal? (... get-by-id 'num2) (build-vector ...)))
      (define postcondition-expr
        `(equal? (eval-lifted (send terminal-info get-terminal-by-id ',output-id))
                 ,output-expr))

      (define (make-grammar-expr stmt expr temps guard type)
        `(grammar terminal-info ,stmt ,expr
                  #:num-temps ,temps #:guard-depth ,guard #:type ,type
                  #:version ',(hash-ref options 'grammar-version)
                  #:choice-version ',(hash-ref options 'grammar-choice)))

      (define program-definition
        (if (send output-node has-sketch? update-name)
            (let ([sketch (send output-node get-sketch update-name)])
              `((define program
                  (make-lifted terminal-info all-operators ',sketch))
                (time
                 (force-type program (Void-type)
                             (lambda (type)
                               ,(make-grammar-expr 2 3 0 0 'type))))))
            `((define program
                (time ,(make-grammar-expr 2 3 0 1 '(Void-type)))))))

      (define (verbose-code code)
        (if (hash-ref options 'verbose?)
            (list code)
            '()))

      (define (run-synthesis)
        (define rosette-code
          `(let ()
             (clear-state!)
             (current-bitwidth ,(hash-ref options 'bitwidth))
             ,@initialization-stmts
             ,@update-stmts
             ,@update-intermediate-stmts
             ,@terminal-info-stmts

             ,@(verbose-code '(displayln "Creating symbolic program"))
             ,@program-definition
             ;; Symbolically run the sampled program
             ,@(verbose-code '(displayln "Running the generated program"))
             (time (eval-lifted program))

             ;; Assert all preconditions
             (define (assert-fn x) (assert x))
             (for-each assert-fn input-assertions)

             (define (assert-pre input)
               (for-each assert-fn (input-preconditions input)))
             (define inputs-list (set->list inputs-set))
             (for-each assert-pre inputs-list)
             
             (define synth
               (time
                (synthesize #:forall (map input-val inputs-list)
                            #:guarantee
                            (begin (assert ,postcondition-expr)
                                   ,@(verbose-code `(displayln "Completed symbolic generation! Running the solver:"))))))
             (and (sat? synth)
                  ,@(verbose-code '(displayln "Solution found! Generating code:"))
                  (let ([code
                         (time (coerce-evaluate (lifted-code program) synth))])
                    (pretty-print code)
                    code))))

        (when (hash-ref options 'debug?) (pretty-print rosette-code))
        (run-in-rosette rosette-code))

      (define (run-metasketch)
        (define module-code
          `(;; TODO: Currently this depends on you running code from
            ;; the right place. Fix.
            (require "rosette/namespace-requires.rkt")
            (provide metasketch)
            (current-bitwidth ,(hash-ref options 'bitwidth))
            ,@initialization-stmts
            ,@update-stmts
            ,@update-intermediate-stmts
            ,@(add-reset-fn 'reset-state!)
            ,@terminal-info-stmts
            (define metasketch
              (grammar-metasketch terminal-info
                                  (set->list inputs-set)
                                  (thunk (list ,postcondition-expr))
                                  reset-state!
                                  ,options))))

        (when (hash-ref options 'debug?) (pretty-print module-code))
        (define module-file (hash-ref options 'module-file))
        (when (file-exists? module-file) (delete-file module-file))
        (with-output-to-file module-file
          (thunk
           (displayln "#lang rosette")
           (for-each writeln module-code)))

        (define synth-code
          `(search #:metasketch ,module-file
                   #:threads 1
                   #:timeout ,(hash-ref options 'timeout)
                   #:bitwidth ,(hash-ref options 'bitwidth)
                   #:verbose #t))

        (when (hash-ref options 'debug?) (pretty-print synth-code))
        (when (hash-ref options 'verbose?)
          (displayln "Starting the metasketch search"))
        (define result (run-in-racket synth-code))
        (and result (lifted-code (second result))))

      (define result
        (if (hash-ref options 'metasketch?)
            (run-metasketch)
            (run-synthesis)))
      (if result
          (begin
            (when (hash-ref options 'debug?) (pretty-print result))
            (send input-relation add-update-code update-name result))
          (begin
            (displayln
             (format "No program found to update ~a upon change ~a to ~a"
                     output-id update-name input-id))
            (send input-relation add-update-code update-name
                  (send output-relation get-update-code 'recompute))))

      (set! intermediate-ids (append intermediate-ids (list output-id))))

    (let ([synthesized-code (send input-relation get-update-code update-name)])
      `(define (,update-name ,@update-args)
         ,define-overwritten-vals
         ,update-code
         ,synthesized-code))))
