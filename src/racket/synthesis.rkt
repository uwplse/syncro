#lang racket

(require "dependency-graph.rkt"
         "namespace.rkt"
         "program.rkt"
         "../rosette/rosette-namespace.rkt"
         "../rosette/grammar/language.rkt"
         "../rosette/types.rkt"
         "../rosette/variable.rkt")

(provide find-update-rules)

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

(define (get-definition node set-id)
  (let ([id (send node get-id)]
        [expr (send node get-fn-code)])
    (if expr `(define ,id ,expr) (symbolic-code node set-id))))

(define (get-node-info node set-id)
  (let ([id (send node get-id)]
        [expr (send node get-fn-code)])
    (list node id (send node get-type) expr (get-definition node set-id)
          (send node get-invariant-code))))

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
        [(list? thing) (map datumify thing)]
        [else thing]))

(define (make-id template . ids)
  (string->symbol (apply format template ids)))

;; Note: Here we use (repr type) to get an expression that
;; evaluates to the type, but we could also store this
;; expression taken straight from the user program.
(define (add-terminal-to-info-code info-sym var-sym type [mutable? #f])
  `(send ,info-sym make-and-add-terminal ',var-sym ,(repr type)
         #:mutable? ,mutable?))

;; Creates the necessary Rosette code for synthesis, runs it, and
;; creates the relevant update functions.
;; prog: A program struct (see read-file.rkt)
(define (find-update-rules prog options)
  ;;;;;;;;;;;;;
  ;; Helpers ;;
  ;;;;;;;;;;;;;

  ;; These are defined inside so that they have access to prog and options.

  (match-define (program constants graph algorithm) prog)

  (define (logging-enabled? option)
    (set-member? (hash-ref options 'logging) option))
  (define (log-for-option option code)
    (if (logging-enabled? option) (list code) '()))
  (define (timing-code code msg)
    (if (logging-enabled? 'stats)
        `(let-values ([(result-from-time cpu-time real-time gc-time)
                       (time-apply (thunk ,code) '())])
           (printf "Took ~ams (~a cpu, ~a gc) to ~a~%"
                   real-time cpu-time gc-time ,msg)
           (first result-from-time))
        code))

  ;; Relevant information about constants
  (match-define (list typed-constant-ids typed-constant-types
                      config-terminal-ids num-configs)
    (get-constant-info constants 'inputs-set))
  
  (define (get-id-info id)
    (get-node-info (get-node graph id) 'inputs-set))

  ;;;;;;;;;;;;;;;
  ;; Synthesis ;;
  ;;;;;;;;;;;;;;;
  (define (perform-synthesis input-id delta-name intermediate-ids output-id)
    (when (logging-enabled? 'progress)
      (printf "Synthesizing update rule for ~a upon delta ~a to ~a~%"
              output-id delta-name input-id))

    ;; Relevant information about the input, intermediates, and output
    (match-define (list input-node _ input-type _ define-input input-invariant)
      (get-id-info input-id))
    (match-define (list _ _ intermediate-types _ define-intermediates _)
      (transpose (map get-id-info intermediate-ids) 6))
    (match-define (list output-node _ output-type output-expr define-output _)
      (get-id-info output-id))
    
    ;; Relevant information for performing symbolic computation on the input
    (match-define (list delta-args delta-defns-code delta-code delta-arg-types)
      (datumify (symbolic-delta-code input-node delta-name 'inputs-set)))

    (define add-terminal-code (curry add-terminal-to-info-code 'terminal-info))

    (define non-output-ids
      (cons input-id
            (append intermediate-ids delta-args typed-constant-ids)))
    (define non-output-types
      (cons input-type
            (append intermediate-types delta-arg-types typed-constant-types)))
    (define add-terminal-stmts
      (cons (add-terminal-code output-id output-type #t)
            (map add-terminal-code non-output-ids non-output-types)))
    (define make-env-expr
      (for/fold ([curr-env-code 'global-environment])
                ([id (cons output-id non-output-ids)])
        `(environment-define ,curr-env-code ',id ,id)))

    ;; TODO: This is misnamed, constants come before this
    (define define-structures
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

    (define (make-grammar-expr stmt expr temps guard type mode sketch?
                               #:terminal-info [info-var 'terminal-info])
      `(grammar ,info-var ,stmt ,expr
                #:num-temps ,temps #:guard-depth ,guard #:type ,type
                #:version ',(if sketch? 'caching (hash-ref options 'grammar-version))
                #:choice-version ',(if sketch? 'basic (hash-ref options 'grammar-choice))
                #:mode ',mode
                #:print-statistics
                ,(set-member? (hash-ref options 'logging) 'stats)))

    (define prederiv-defn-code
      `(,@(log-for-option 'progress '(displayln "Generating the prederivative"))
        (define prederiv
          ,(timing-code (make-grammar-expr 1 1 0 0 (Any-type) '(tmps 2) #f)
                        "generate the prederivative"))))
    (define prederiv-run-code
      `((define initial-env ,make-env-expr)
        (define env-after-prederiv
          ,(timing-code
            '(let loop ([code-lst prederiv] [loop-env initial-env])
               (if (null? code-lst)
                   loop-env
                   (let ([new-env (second (eval-lifted (car code-lst) loop-env))])
                     (loop (cdr code-lst) new-env))))
            "run the prederivative"))))

    ;; Example: (set! num2helper (build-vector ...))
    ;; Taken straight from the user program, but operates on
    ;; symbolic variables.
    ;; Note that this must come after the delta.
    ;; TODO: For now, we have to define intermediates before
    ;; the delta in case the output relation depends on
    ;; them. So here, we should use a set!
    (define program-definition
      `(,@(log-for-option 'progress '(displayln "Creating symbolic program"))
        ,@(if (send output-node has-sketch? delta-name)
              (let ([sketch (send output-node get-sketch delta-name)])
                `((define program
                    (make-lifted terminal-info all-operators ',sketch))
                  ,(timing-code
                    `(force-type program (Void-type)
                                 (lambda (info type)
                                   ,(make-grammar-expr 2 2 0 0 'type 'stmt #t
                                                       #:terminal-info 'info)))
                    "generate the sketch")))
              `((define program
                  ,(timing-code
                    (make-grammar-expr 2 2 0 1 '(Void-type) 'stmt #f)
                    "generate the postderivative"))))))
    (define program-run-code
      `((define env-for-postderiv
          ,(for/fold ([curr-env-code `(environment-set env-after-prederiv ',input-id ,input-id)])
                     ([def define-intermediates])
             `(environment-set ,curr-env-code ',(cadr def) (begin ,@(cddr def)))))
        (define final-env
          ,(timing-code '(second (eval-lifted program env-for-postderiv))
                        "run the postderivative"))))

    (define postcondition-expr
      `(let ([output-terminal (send terminal-info get-terminal-by-id ',output-id)])
         (define output-value
           (first (eval-lifted output-terminal final-env)))
         (equal? output-value ,output-expr)))

    (define (get-code-for-config i)
      `(let ()
         (define inputs-set (mutable-set))
         ;; Example: (define NUM_WORDS 12)
         ,@(map (lambda (c) (constant-value-code c i 'inputs-set))
                constants)
         (for-each set-configurable-value!
                   configurables
                   (list ,@config-terminal-ids))
         ,@define-structures
         ,delta-defns-code
         ,@(log-for-option
            'progress
            `(printf "Running the prederivative for configuration ~a~%"
                     ,(add1 i)))
         ,@prederiv-run-code
         ,delta-code
         (define input-assertion-after-delta ,input-invariant)

         ;; Symbolically run the sampled program
         ,@(log-for-option
            'progress
            `(printf "Running the generated program for configuration ~a~%"
                     ,(add1 i)))
         ,@program-run-code

         ;; Assert all preconditions
         (define (assert-fn x) (assert x))
         (assert input-assertion)
         (assert input-assertion-after-delta)

         (define (assert-pre input)
           (for-each assert-fn (input-preconditions input)))
         (define inputs-list (set->list inputs-set))
         (for-each assert-pre inputs-list)

         ;; Note: It is not (equal? ,output-id ,output-expr)
         ;; because when we run the lifted program, it modifies
         ;; the value *stored in the lifted program*, which is not
         ;; necessarily the same as the value in ,output-id.
         ;; Example: (assert (equal? (... get-by-id 'num2) (build-vector ...)))
         (define new-output
           (first
            (eval-lifted
             (send terminal-info get-terminal-by-id ',output-id) final-env)))
         (define expected-result ,output-expr)
         (define postcondition (equal? new-output expected-result))

         (define (print-cex model)
           (define (get-value thing)
             (coerce-evaluate thing model))
           (and (not (coerce-evaluate postcondition model))
                (let* ([updated-input (get-value ,input-id)]
                       [updated-output (get-value new-output)]
                       [delta-args (map get-value (list ,@delta-args))]
                       [expected-output (get-value expected-result)]
                       [update-code (cons ',delta-name delta-args)])
                  (printf "  After update ~a, we have:~%  ~a: ~a~%  Actual ~a:   ~a~%  Expected ~a: ~a~%"
                          update-code
                          (symbol->string ',input-id)  updated-input
                          (symbol->string ',output-id) updated-output
                          (symbol->string ',output-id) expected-output)
                  #f)))

         (list inputs-list postcondition print-cex)))

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
           (define terminal-info (new Lexical-Terminal-Info%))
           ,@add-terminal-stmts
           ,@prederiv-defn-code
           ,@program-definition
           (define results-for-config
             ,(cons 'list
                    (for/list ([i num-configs])
                      (get-code-for-config i))))

           (define (get-code model)
             (let* ([pre-result (coerce-evaluate prederiv model)]
                    [post-result (coerce-evaluate program model)])
               (let-values ([(cleaned-prederiv cleaned-postderiv)
                             (eliminate-dead-code pre-result post-result)])
                 (append '(let ())
                         (map lifted-code cleaned-prederiv)
                         (list ',delta-code
                               (lifted-code cleaned-postderiv))))))

           (define (print-program model)
             (displayln "Found a potential program:")
             (pretty-print (get-code model)))
           (define (print-cex model)
             (displayln "Found a counterexample:")
             ;; Each configuration has its own counterexample printer.
             ;; Call each one until one succeeds.
             (for/or ([triple results-for-config])
               ((third triple) model)))

           (define synth
             ,(timing-code
               `(synthesize-with-printers
                 #:forall (map input-val
                               (foldl append '()
                                      (map first results-for-config)))
                 #:guarantee
                 (begin (for ([pair results-for-config])
                          (assert (second pair)))
                        ,@(log-for-option 'progress `(displayln "Completed symbolic generation! Running the solver:")))
                 #:printers
                 [,(if (logging-enabled? 'cegis) 'print-program '(const #t))
                  ,(if (logging-enabled? 'cegis) 'print-cex '(const #t))])
               "solve the formula"))
           (and (sat? synth)
                ,@(log-for-option
                   'progress
                   '(displayln "Solution found! Generating code:"))
                (let ([code (get-code synth)])
                  ,@(log-for-option 'progress '(pretty-print code))
                  code))))

      (when (logging-enabled? 'debug) (pretty-print rosette-code))
      (run-in-rosette rosette-code))

    ;;;;;;;;;;;;;;;;;;
    ;; Metasketches ;;
    ;;;;;;;;;;;;;;;;;;

    ;; Defines a reset function that when called will reset all data
    ;; structures to their state at the point where the function was defined.
    (define (add-reset-fn name)
      `((define init-state (clone (list ,input-id ,output-id ,@intermediate-ids)))
        (define (,name)
          (define clone-state (clone init-state))
          ,@(for/list ([id (cons input-id (cons output-id intermediate-ids))]
                       [index (in-naturals 0)])
              `(set! ,id (list-ref clone-state ,index))))))

    (define (run-metasketch)
      (define module-code
        `(;; TODO: Currently this depends on you running code from
          ;; the right place. Fix.
          (require "rosette/namespace-requires.rkt")
          (provide metasketch)
          (current-bitwidth ,(hash-ref options 'bitwidth))
          ,@define-structures
          ,delta-defns-code
          ,@add-terminal-stmts
          ,@prederiv-defn-code
          ,@prederiv-run-code
          ,delta-code
          ,@(add-reset-fn 'reset-state!)
          (define metasketch
            (grammar-metasketch terminal-info
                                (set->list inputs-set)
                                (thunk (list ,postcondition-expr))
                                reset-state!
                                ,options))))

      (when (logging-enabled? 'debug) (pretty-print module-code))
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

      (when (logging-enabled? 'debug) (pretty-print synth-code))
      (when (logging-enabled? 'progress)
        (displayln "Starting the metasketch search"))
      (define result (run-in-racket synth-code))
      (and result (lifted-code (second result))))

    ;; End of Rosette code

    ;;;;;;;;;;;;;
    ;; Results ;;
    ;;;;;;;;;;;;;

    (define result
      (if (hash-ref options 'metasketch?)
          (run-metasketch)
          (run-synthesis)))
    (if result
        (send input-node add-delta-code delta-name result)
        (begin
          (displayln
           (format "No program found to update ~a upon delta ~a to ~a"
                   output-id delta-name input-id))
          (send input-node add-delta-code delta-name
                (send output-node get-delta-code 'recompute)))))

  ;;;;;;;;;;;;;;;;;
  ;; Outer loops ;;
  ;;;;;;;;;;;;;;;;;

  (define (updates-for-input-and-delta input-id delta-name)
    (define input-node (get-node graph input-id))
    (define check-path-fn (check-path? graph input-id))
    (define intermediate-ids '())
    (for ([output-id (get-ids graph)] #:unless (equal? input-id output-id))
      (when (check-path-fn output-id)
        (perform-synthesis input-id delta-name intermediate-ids output-id))
      (set! intermediate-ids (append intermediate-ids (list output-id))))

    (match-define (list delta-args _ delta-code _)
      (datumify (symbolic-delta-code input-node delta-name 'inputs-set)))

    (let ([synthesized-code (send input-node get-delta-code delta-name)])
      `(define (,delta-name ,@delta-args)
         ,delta-code
         ,synthesized-code)))

  (for/fold ([result null]) ([input-id (get-ids graph)])
    (define input-node (get-node graph input-id))
    (define updates
      (for/list ([delta-name (send input-node get-delta-names)])
        (updates-for-input-and-delta input-id delta-name)))
    (append updates result)))
