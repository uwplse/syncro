#lang racket

(require "dependency-graph.rkt"
         "namespace.rkt"
         "program.rkt"
         "../rosette/rosette-namespace.rkt"
         "../rosette/grammar/language.rkt"
         "../rosette/types.rkt"
         "../rosette/variable.rkt")

(provide perform-synthesis)

;; Returns code that creates a symbolic value of the given node. If
;; varset-name is a symbol, the generated code will add all symbolic
;; variables to varset-name, which at runtime will be a set. If
;; varset-name is #f, that does not happen.
(define (symbolic-code node [varset-name #f])
  (let ([type (send node get-type)]
        [var (send node get-id)])
    `(define ,var (make-symbolic ,(repr type) ,varset-name))))

(define (symbolic-delta-code node delta-name set-id)
  (let* ([delta-args (send node get-delta-args delta-name)]
         [arg-names (map variable-symbol delta-args)]
         [arg-types (map variable-type delta-args)]
         [delta-body (send node get-delta-body delta-name)])
    (define arg-defns
      (map (lambda (name type)
             `(define ,name (make-symbolic ,(repr type) ,set-id)))
           arg-names arg-types))
    (list arg-names
          `(begin ,@arg-defns)
          `(begin ,@delta-body)
          arg-types)))

(define (constant-initialization-code c)
  (define sym (variable-symbol c))
  (cond [(not (constant-for-types? c))
         `(define ,sym 'placeholder)]
        [(config-constant? c)
         `(define ,sym (make-configurable ',sym))]
        [(variable-has-expression? c) 
         (variable-definition c)]
        [else
         (error (format "Internal error: Constant ~a can't be initialized"
                        c))]))

(define (constant-value-code c config-num varset-name)
  (cond [(config-constant? c)
         (let ([sym (variable-symbol c)]
               [exprs (constant-configs c)])
           `(set! ,sym ,(list-ref exprs config-num)))]
        [(variable-has-expression? c) 
         (variable-set!-code c)]
        [else
         `(set! ,(variable-symbol c)
                (make-symbolic ,(repr (variable-type c)) ,varset-name))]))

;; Various helpers that simply get information out of data structures.
(define (get-constant-info constants varset-name)
  (let* ([typed-vars (filter variable-has-type? constants)]
         [configs (filter config-constant? constants)]
         [nums (remove-duplicates
                (map (compose length constant-configs) configs))])
    (unless (<= (length nums) 1)
      (error (format "All configurations must have the same length! Given program has ~a"
                     nums)))

    (list (map variable-symbol typed-vars)
          (map variable-type typed-vars)
          (map variable-symbol configs)
          (if (null? nums) 1 (first nums)))))

(define (get-node-info node)
  (let ([id (send node get-id)]
        [expr (send node get-fn-code)])
    (list node id (send node get-type) expr
          `(define ,id ,expr) (send node get-invariant-code))))

(define (get-symbolic-info node delta-name set-id)
  (append (list (symbolic-code node set-id))
          (symbolic-delta-code node delta-name set-id)))

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

  (define constants (program-constants prog))
  ;; Relevant information about constants
  (match-define (list constant-terminal-ids constant-terminal-types
                      config-terminal-ids num-configs)
    (get-constant-info constants 'inputs-set))
  
  (define (get-id-info id)
    (get-node-info (get-node graph id)))
  
  ;; Relevant information about the input relation
  (match-define (list input-relation input-id input-type _ _ input-invariant)
    (get-id-info (car (get-ids graph))))

  (for/list ([delta-name (send input-relation get-delta-names)])
    
    ;; Relevant information for performing symbolic computation on the input
    (match-define (list define-input delta-args delta-defns-code delta-code delta-arg-types)
      (datumify
       (get-symbolic-info input-relation delta-name 'inputs-set)))

    (define intermediate-ids '())
    (for ([output-id (cdr (get-ids graph))])
      (printf "Synthesizing update rule for ~a upon delta ~a to ~a~%"
              output-id delta-name input-id)
      
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
        `(send terminal-info make-and-add-terminal ',var ,(repr type)
               #:mutable? ,mutable?))
      (define (set-terminal-code var)
        `(send terminal-info set-value ',var ,var))

      (define all-ids-except-output
        (append (list input-id) intermediate-ids
                delta-args constant-terminal-ids))
      (define all-types-except-output
        (append (list input-type) intermediate-types
                delta-arg-types constant-terminal-types))
      (define add-terminal-stmts
        (cons (add-terminal-code output-id output-type #:mutable? #t)
              (map add-terminal-code
                   all-ids-except-output all-types-except-output)))
      (define set-terminal-stmts
        (map set-terminal-code (cons output-id all-ids-except-output)))

      ;; TODO: This is misnamed, constants come before this
      (define initialization-stmts
        `(;; Example: (define word->topic (build-vector 12 ...))
          ;; The resulting data structure contains symbolic variables.
          ,define-input
          (define input-assertion ,input-invariant)
          ;; Example: (define num2helper (build-vector ...))
          ;; Taken straight from the user program, but operates on
          ;; symbolic variables.
          ;; This needs to be done here in case the output depends on
          ;; intermediate relations.
          ,@define-intermediates
          ;; Example: (define num2 (build-vector ...))
          ;; Basically the same as for intermediates.
          ,define-output))

      (define (make-grammar-expr stmt expr temps guard type mode)
        `(grammar terminal-info ,stmt ,expr
                  #:num-temps ,temps #:guard-depth ,guard #:type ,type
                  #:version ',(hash-ref options 'grammar-version)
                  #:choice-version ',(hash-ref options 'grammar-choice)
                  #:mode ',mode))

      (define prederiv-defn-code
        `((displayln "Generating the prederivative")
          (define prederiv
            (time ,(make-grammar-expr 1 1 0 0 (Any-type) '(tmps 2))))))
      (define prederiv-run-code
        `(,@set-terminal-stmts
          (displayln "Running the prederivative")
          (time (for-each eval-lifted prederiv))))

      ;; Example: (set! num2helper (build-vector ...))
      ;; Taken straight from the user program, but operates on
      ;; symbolic variables.
      ;; Note that this must come after the delta.
      ;; TODO: For now, we have to define intermediates before
      ;; the delta in case the output relation depends on
      ;; them. So here, we should use a set!
      (define update-intermediate-stmts
        (map (lambda (code)
               `(send terminal-info set-value ',(cadr code) ,@(cddr code)))
             define-intermediates))

      ;; Defines a reset function that when called will reset all data
      ;; structures to their state at the point where the function was defined.
      (define (add-reset-fn name)
        `((define init-state (clone (list ,input-id ,output-id ,@intermediate-ids)))
          (define (,name)
            (define clone-state (clone init-state))
            ,@(for/list ([id (cons input-id (cons output-id intermediate-ids))]
                         [index (in-naturals 0)])
                `(set! ,id (list-ref clone-state ,index))))))

      ;; Note: It is not (equal? ,output-id ,output-expr)
      ;; because when we run the lifted program, it modifies
      ;; the value *stored in the lifted program*, which is not
      ;; necessarily the same as the value in ,output-id.
      ;; Example: (assert (equal? (... get-by-id 'num2) (build-vector ...)))
      (define postcondition-expr
        `(equal? (eval-lifted (send terminal-info get-terminal-by-id ',output-id))
                 ,output-expr))

      (define (verbose-code code)
        (if (hash-ref options 'verbose?)
            (list code)
            '()))

      (define program-definition
        `(,@(verbose-code '(displayln "Creating symbolic program"))
          ,@(if (send output-node has-sketch? delta-name)
                (let ([sketch (send output-node get-sketch delta-name)])
                  `((define program
                      (make-lifted terminal-info all-operators ',sketch))
                    (time
                     (force-type program (Void-type)
                                 (lambda (type)
                                   ,(make-grammar-expr 2 2 0 0 'type 'stmt))))))
                `((define program
                    (time ,(make-grammar-expr 2 2 0 1 '(Void-type) 'stmt)))))))
      (define program-run-code
        `(,@set-terminal-stmts
          (time (eval-lifted program))))

      (define (get-code-for-config i)
        `(let ()
           (define inputs-set (mutable-set))
           ;; Example: (define NUM_WORDS 12)
           ,@(map (lambda (c) (constant-value-code c i 'inputs-set))
                  constants)
           (for-each set-configurable-value!
                     configurables
                     (list ,@config-terminal-ids))
           ,@initialization-stmts
           ,delta-defns-code
           ,@prederiv-run-code
           ,delta-code
           (define input-assertion-after-delta ,input-invariant)
           ,@update-intermediate-stmts

           ;; Symbolically run the sampled program
           ,@(verbose-code '(displayln "Running the generated program"))
           ,@program-run-code

           ;; Assert all preconditions
           (define (assert-fn x) (assert x))
           (assert input-assertion)
           (assert input-assertion-after-delta)

           (define (assert-pre input)
             (for-each assert-fn (input-preconditions input)))
           (define inputs-list (set->list inputs-set))
           (for-each assert-pre inputs-list)

           (list inputs-list ,postcondition-expr)))

      (define (run-synthesis)
        ;; Every variable in terminal-info has a type, which could
        ;; depend on a number in a configuration (for example, the
        ;; Word type which has NUM_WORDS items).
        ;; However, the terminal info must be defined separately from
        ;; any configuration, because the sampled program must work
        ;; for all configurations. And the program generation process
        ;; does need the types, though not things like NUM_WORDS.
        ;; We solve this by making types configurable. See types.rkt.
        (define rosette-code
          `(let ()
             (clear-state!)
             (current-bitwidth ,(hash-ref options 'bitwidth))
             ,@(map constant-initialization-code constants)
             (define configurables (list ,@config-terminal-ids))
             (define terminal-info (new Terminal-Info%))
             ,@add-terminal-stmts
             ,@prederiv-defn-code
             ,@program-definition
             (define inputs-and-postconditions
               ,(cons 'list
                      (for/list ([i num-configs])
                        (get-code-for-config i))))
             (define synth
               (time
                (synthesize
                 #:forall (map input-val
                               (foldl append '()
                                      (map first inputs-and-postconditions)))
                 #:guarantee
                 (begin (for ([pair inputs-and-postconditions])
                          (assert (second pair)))
                        ,@(verbose-code `(displayln "Completed symbolic generation! Running the solver:"))))))
             (and (sat? synth)
                  ,@(verbose-code '(displayln "Solution found! Generating code:"))
                  (let* ([result (time (coerce-evaluate program synth))]
                         [prederiv-result (time (coerce-evaluate prederiv synth))]
                         [code (append '(let ())
                                       (map lifted-code prederiv-result)
                                       (list 'delta-goes-here)
                                       (list (lifted-code (eliminate-dead-code result))))])
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
            ,delta-defns-code
            ,@add-terminal-stmts
            ,@prederiv-defn-code
            ,@prederiv-run-code
            ,delta-code
            ,@update-intermediate-stmts
            ,@(add-reset-fn 'reset-state!)
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
            (send input-relation add-delta-code delta-name result))
          (begin
            (displayln
             (format "No program found to update ~a upon delta ~a to ~a"
                     output-id delta-name input-id))
            (send input-relation add-delta-code delta-name
                  (send output-relation get-delta-code 'recompute))))

      (set! intermediate-ids (append intermediate-ids (list output-id))))

    (let ([synthesized-code (send input-relation get-delta-code delta-name)])
      `(define (,delta-name ,@delta-args)
         ,delta-code
         ,synthesized-code))))
