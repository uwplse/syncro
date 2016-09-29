#lang racket

(require "dependency-graph.rkt" "rosette-namespace.rkt"
         "types.rkt" "variables.rkt")

(provide perform-synthesis)

(define (get-constant-info vars)
  (let ([typed-vars (filter typed-variable? vars)])
    (list (map variable-definition vars)
          (map variable-symbol typed-vars)
          (map variable-type typed-vars))))

(define (get-node-info node)
  (let ([id (send node get-id)]
        [expr (send node get-fn-code)])
    (list node id (send node get-type) expr
          `(define ,id ,expr))))

(define (get-symbolic-info node update-type #:varset-id [set-id #f])
  (let* ([update-args (send node get-update-arg-names update-type)]
         [symbolic-update (send node get-symbolic-update-code update-type update-args)])
    (append (list (send node get-symbolic-code set-id))
            (list update-args)
            symbolic-update
            (send/apply node get-old-values-code update-type update-args))))

(define (transpose list-of-lists len)
  (unless (or (null? list-of-lists)
              (= len (length (car list-of-lists))))
    (error "Invalid arguments to transpose: ~a and ~a~%" list-of-lists len))
  (foldr (lambda (x y) (map cons x y))
         (build-list len (lambda (x) '()))
         list-of-lists))

(define (datumify thing)
  (cond [(syntax? thing) (syntax->datum thing)]
        [(list? thing)
         (map datumify thing)]
        [else thing]))

(define (make-id template . ids)
  (string->symbol (apply format template ids)))

(define (perform-synthesis constants graph)
  (define (get-id-info id)
    (get-node-info (get-node graph id)))
  
  ;; Relevant information about constants
  (match-define (list define-constants constant-terminal-ids constant-terminal-types)
    (get-constant-info constants))
  
  ;; Relevant information about the input relation
  (match-define (list input-relation input-id input-type _ _)
    (get-id-info (car (get-ids graph))))

  (for/list ([update-type (send input-relation get-update-types)])
    
    ;; Relevant information for performing symbolic computation on the input
    ;; Note: It's important to call this only once per update
    ;; type so that the variables have consistent names.
    (match-define (list define-input update-args update-defns-code update-code update-arg-types define-overwritten-vals overwritten-vals overwritten-vals-types)
      (datumify
       (get-symbolic-info input-relation update-type
                          #:varset-id 'symbolic-vars)))
    
    (define intermediate-ids '())
    (for ([output-id '(num1 num2helper num2) #;(cdr (get-ids graph))])
      (printf "Synthesizing update rule for ~a upon update ~a to ~a~%"
              output-id update-type input-id)
      
      ;; Relevant information about intermediate relations
      ;; Does a lot of recomputation, but not a bottleneck
      (match-define (list intermediate-relations _ intermediate-types _ define-intermediates)
        (transpose (map get-id-info intermediate-ids) 5))

      ;; Relevant information about the output relation
      (match-define (list output-relation _ output-type output-expr define-output)
        (get-id-info output-id))

      (define (add-terminal-code var type #:mutable [mutable #f])
        ;; Note: Here we use (repr type) to get an expression that
        ;; evaluates to the type, but we could also store this
        ;; expression taken straight from the user program.
        `(send terminal-info add-terminal ',var ,var ,(repr type)
               #:mutable ,mutable))

      (define add-terminals
        (map add-terminal-code
             (append (list input-id) intermediate-ids
                     overwritten-vals update-args
                     constant-terminal-ids)
             (append (list input-type) intermediate-types
                     overwritten-vals-types update-arg-types
                     constant-terminal-types)))
      
      (define rosette-code
        `(let ()
           ;; Example: (define NUM_WORDS 12)
           ,@define-constants
           
           (define symbolic-vars (mutable-set))

           ;; Example: (define word->topic (build-vector 12 ...))
           ;; The resulting data structure contains symbolic variables.
           ,define-input

           ;; Example: (define num2 (build-vector ...))
           ;; Basically the same as for intermediates.
           ,define-output

           ;; Example:
           ;; (define-symbolic* index9079 integer?)
           ;; (assert (>= index9079 0))
           ;; (assert (< index9079 12))
           ;; (define-symbolic* val9080 integer?)
           ;; (assert (>= val9080 0))
           ;; (assert (< val9080 3))
           ,update-defns-code

           ;; Example:
           ;; (define old-value9082 (vector-ref word->topic index9079))
           ,define-overwritten-vals

           ;; Example: (vector-set! word->topic index9079 val9080)
           ,update-code

           ;; Example: (define num2helper (build-vector ...))
           ;; Taken straight from the user program, but operates on
           ;; symbolic variables.
           ;; Note that this must come after the update.
           ,@define-intermediates

           ;; Create the grammar and sample a program
           (define terminal-info (new Terminal-Info%))
           ,(add-terminal-code output-id output-type #:mutable #t)
           ,@add-terminals
           (define program (grammar terminal-info 3 4 #:num-temps 0 #:guard-depth 1))

           (define synth
             (time
              (synthesize
               ;; For every possible input relation (captured in
               ;; symbolic vars) and every possible update to that
               ;; relation
               #:forall (append (list ,@update-args)
                                (set->list symbolic-vars))
               #:guarantee
               (begin
                 ;; Symbolically run the sampled program
                 (eval-lifted program)
                 ;; Example: (assert (equal? num2 (build-vector ...)))
                 (assert (equal? ,output-id ,output-expr))))))
           
           (and (sat? synth)
                (evaluate (lifted-code program) synth))))

      ;(pretty-print rosette-code)
      (define result (run-in-rosette rosette-code))
      (if result
          (begin
            (pretty-print result)
            (send input-relation add-update-code update-type result))
          (begin
            (display "No program found\n")
            (send input-relation add-update-code update-type
                  `(set! ,output-id ,(send output-relation get-fn-code)))))

      (set! intermediate-ids (append intermediate-ids (list output-id))))

    (let ([update-id (make-id "~a-~a!" update-type input-id)]
          [synthesized-code (send input-relation get-update-code update-type)])
      `(define (,update-id ,@update-args)
         ,define-overwritten-vals
         ,update-code
         ,synthesized-code))))
