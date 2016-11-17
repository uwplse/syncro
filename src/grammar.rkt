#lang rosette

(require racket/syntax rosette/lib/angelic)

(require "alist.rkt" "operators.rkt" "rosette-util.rkt" "types.rkt" "variable.rkt")

(provide grammar Terminal-Info% eval-lifted lifted-code)

;; Counts the number of symbolic booleans created during a run of the grammar.
(define num-vars-without-filtering (make-parameter 0))
(define num-boolean-vars (make-parameter 0))
(define num-vars-if-optimized (make-parameter 0))

(define (fold-expr f expr default)
  (if (and (not (term? expr)) (integer? expr))
      expr
      (match expr
        [(expression op child ...)
         (apply f (map (lambda (e) (fold-expr f e default))
                       child))]
        [(term content type)
         default])))

(define (get-max-from-expr expr [default 0])
  (fold-expr max expr default))

(define (get-sum-from-expr expr)
  (fold-expr + expr 0))

;; Takes a list of lifted programs and produces a value representing a
;; choice of exactly one of them.
(define (my-choose* . args)
  ;; TODO: Following line sometimes speeds it up, sometimes slows it down.
  (define valid-args (filter (compose not lifted-error?) args))
  (if (null? valid-args)
      (lifted-error)
      (let* ([symbolic-num-new (length valid-args)])
        (num-vars-without-filtering (+ (length args) -1
                                       (num-vars-without-filtering)))
        (num-boolean-vars (+ (get-sum-from-expr symbolic-num-new) -1
                             (num-boolean-vars)))
        (num-vars-if-optimized (+ (get-max-from-expr symbolic-num-new) -1
                                  (num-vars-if-optimized)))
        (apply choose* valid-args))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Lifting operators ;;
;;;;;;;;;;;;;;;;;;;;;;;

(match-define (list alpha-v1 alpha-v2 alpha-v3 beta-v3 alpha-v4 beta-v4 alpha-v5)
  (build-list 7 (lambda (i) (Type-var))))

(define cmp-type
  (Procedure-type (list (Integer-type) (Integer-type)) (Boolean-type)))
(define arith-type
  (Procedure-type (list (Integer-type) (Integer-type)) (Integer-type)))

(define-lifted
  [void void^ (Procedure-type '() (Void-type))]
  [vector-increment! vector-increment!^
                     (Procedure-type (list (Vector-type alpha-v1 (Integer-type))
                                           alpha-v1)
                                     (Void-type))]
  [vector-decrement! vector-decrement!^
                     (Procedure-type (list (Vector-type alpha-v2 (Integer-type))
                                           alpha-v2)
                                     (Void-type))]
  [vector-set! vector-set!^
               (Procedure-type (list (Vector-type alpha-v3 beta-v3)
                                     alpha-v3
                                     beta-v3)
                               (Void-type))]
  [vector-ref vector-ref^
              (Procedure-type (list (Vector-type alpha-v4 beta-v4)
                                    alpha-v4)
                              beta-v4)]
  [equal? equal?^ (Procedure-type (list alpha-v5 alpha-v5) (Boolean-type))]
  [= =^ cmp-type] [< <^ cmp-type]
  [+ +^ arith-type] [- -^ arith-type] [* *^ arith-type] #;[/ /^ arith-type])

(struct special-form (name constructor) #:transparent)
(define operator-info
  (list void^ vector-increment!^ vector-decrement!^ vector-set!^
        vector-ref^ equal?^ =^ <^ +^ -^ *^
        (special-form 'if if^) (special-form 'set! set!^)))

(define (can-have-output-type? operator type)
  (cond [(not (special-form? operator))
         (let ([range-type (Procedure-range-type (variable-type operator))])
           (or (Type-var? range-type)
               (is-supertype? type range-type)))]
        [(equal? (special-form-name operator) 'if)
         #t]
        [(equal? (special-form-name operator) 'set!)
         (is-supertype? type (Void-type))]
        [else
         (error (format "Unknown special form: ~a" operator))]))

(define (get-operators-with-output-type type)
  (filter (lambda (op) (can-have-output-type? op type))
          operator-info))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grammar construction ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; terminal-info: A Terminal-Info object
;; num-stmts:     Number of statements to allow
;; depth:         Expression depth to allow
;; #:num-temps:   Number of temporary variables to add to the sketch
;; #:guard-depth: Depth of the guard expression. If this is #f, no
;;                guard is inserted.
;; Note that the num-stmts and depth do not have exact meanings, they
;; are simply used as costs. (For example, despite an if having
;; multiple statements inside it, it counts as only one statement.)
(define (grammar terminal-info num-stmts depth
                 #:num-temps [num-temps 0]
                 #:guard-depth [guard-depth #f])

  (num-vars-without-filtering 0)
  (num-boolean-vars 0)
  (num-vars-if-optimized 0)
  
  (define proc #;grammar-sharing grammar-basic)
  (define result
    (proc terminal-info num-stmts depth #:num-temps num-temps
          #:guard-depth guard-depth))
  
  (printf "Used ~a boolean variables~%Without filtering would be ~a~%If optimized would be ~a~%"
          (num-boolean-vars)
          (num-vars-without-filtering)
          (num-vars-if-optimized))
  result)
            
(define (grammar-sharing terminal-info num-stmts depth
                         #:num-temps [num-temps 0]
                         #:guard-depth [guard-depth #f])

  (define if-type
    (let ([if-alpha (Type-var)])
      (Procedure-type (list (Boolean-type) if-alpha if-alpha)
                      if-alpha)))
  
  (define (make-subexp-if type->subexp desired-type depth)
    (match-define (list subexps new-type->subexp)
      (make-subexp-helper if-type type->subexp desired-type depth))
    (list (apply if^ subexps) new-type->subexp))
  
  (define (make-subexp-set! type->subexp desired-type depth)
    (let* ([variable
            (apply my-choose*
                   (send terminal-info get-terminals #:mutable? #t))]
           [type (infer-type variable)])
      (if (alist-has-key? type->subexp type)
          (list (set!^ variable (alist-get type->subexp type))
                type->subexp)
          (let ([subexp (general-grammar type depth)])
            (list (set!^ variable subexp)
                  (alist-insert type->subexp type subexp))))))

  (define (make-subexp-proc proc type->subexp desired-type depth)
    (match-define (list subexps new-type->subexp)
      (make-subexp-helper (variable-type proc) type->subexp desired-type depth))
    (list (apply proc subexps) new-type->subexp))
  
  (define (make-subexp-helper operator-type type->subexp desired-type depth)
    (let ([domain (get-domain-given-range operator-type desired-type)]
          [type->subexp-copy type->subexp]
          [mapping (make-type-map)])
      
      (define (get-or-make-subexp type)
        (define concrete-type
          (replace-type-vars type mapping (Any-type)))

        (match-define (list subexp new-copy)
          (if (alist-has-key? type->subexp-copy concrete-type)
              (alist-get-and-remove type->subexp-copy concrete-type)
              (let ([res (general-grammar concrete-type (- depth 1))])
                (set! type->subexp
                      (alist-insert type->subexp concrete-type res))
                (list res type->subexp-copy))))

        (set! type->subexp-copy new-copy)
        ;; The value of this unify can be seen in if^, where if the
        ;; first expression chosen is of type Int, this will force the
        ;; second expression to also have type Int
        (unless (lifted-error? subexp)
          (unify type (infer-type subexp) mapping))
        subexp)
      
      (list (map get-or-make-subexp domain) type->subexp)))
    
  (define (make-subexp operator type->subexp desired-type depth)
    (cond [(not (special-form? operator))
           (make-subexp-proc operator type->subexp desired-type depth)]
          [(equal? (special-form-name operator) 'if)
           (make-subexp-if type->subexp desired-type depth)]
          [(equal? (special-form-name operator) 'set!)
           (make-subexp-set! type->subexp desired-type depth)]
          [else
           (error (format "Unknown special form: ~a" operator))]))

  ;; Type can be symbolic. This may make things tricky.
  ;; TODO: Is this sound? There may be some programs that we should
  ;; generate but don't, specifically when the desired-type is
  ;; symbolic and so we have too much sharing.
  ;; TODO: Handle mutability correctly
  (define (general-grammar desired-type depth #:mutable? [mutable? #t])
    (define operators (get-operators-with-output-type desired-type))

    (define type->subexp (make-alist))
    ;; Here we are relying on map to process elements sequentially. I
    ;; think this is guaranteed by map, but am not sure.
    ;; It would also be okay for map to process things in an arbitrary
    ;; order, but still sequentially. Parallelism would be bad though.
    (apply my-choose*
           (append
            ;; Base case: Terminals
            (send terminal-info get-terminals #:type desired-type
                  #:mutable? mutable?)
            (if (is-supertype? desired-type (Integer-type))
                (list (let () (define-symbolic* hole integer?) hole))
                '())
            (if (= depth 0)
                '()
                (map
                 (lambda (operator)
                   (match-let ([(list result new-type->subexp)
                                (make-subexp operator type->subexp desired-type depth)])
                     (set! type->subexp new-type->subexp)
                     result))
                 operators)))))

  (build-grammar terminal-info num-stmts depth num-temps guard-depth
                 general-grammar
                 (lambda (num-stmts depth)
                   (build-list num-stmts
                               (lambda (i)
                                 (general-grammar (Void-type) depth))))))

(define (grammar-basic terminal-info num-stmts depth
                       #:num-temps [num-temps 0]
                       #:guard-depth [guard-depth #f])  
  (define (stmt-grammar num-stmts depth)
    (if (= num-stmts 0)
        (void^)
        (my-choose* (void^)
                    (begin^ (base-stmt-grammar num-stmts depth)
                            (stmt-grammar (- num-stmts 1) depth)))))

  ;; Vector statements, set!, and if
  (define (base-stmt-grammar num-stmts depth)
    ;; TODO: boolean-expr-grammar should have some other depth
    (if (<= num-stmts 2)
        (my-choose* (vector-stmt-grammar depth)
                    (set!-stmt-grammar depth))
        (my-choose* (vector-stmt-grammar depth)
                    (set!-stmt-grammar depth)
                    (if^ (expr-grammar (Boolean-type) depth)
                         (stmt-grammar 1 depth)
                         (stmt-grammar 1 depth)))))

  ;; vector-set!, vector-increment!, vector-decrement!
  (define (vector-stmt-grammar depth)
    (let* ([vec (expr-grammar (Vector-type (Bottom-type) (Any-type))
                              (- depth 1) #:mutable? #t)]
           [vec-type (infer-type vec)])
      (if (Error-type? vec-type)
          (lifted-error)
          (let* ([index (expr-grammar (Vector-index-type vec-type) (- depth 1))]
                 [value (expr-grammar (Vector-output-type vec-type) (- depth 1))])
            (my-choose* ((my-choose* vector-increment!^ vector-decrement!^) vec index)
                        (vector-set!^ vec index value))))))

  (define (set!-stmt-grammar depth)
    (let* ([variable
            (apply my-choose*
                   (send terminal-info get-terminals #:mutable? #t))])
      (set!^ variable (expr-grammar (infer-type variable) depth)))) 

  ;; If #:mutable is #t, then the return value must be mutable.
  ;; If #:mutable is #f, then the return value may or may not be mutable.
  (define (expr-grammar desired-type depth #:mutable? [mutable? #f])
    (let ([base (apply my-choose*
                       (send terminal-info get-terminals
                             #:type desired-type
                             #:mutable? mutable?))])
      (if (= depth 0)
          base
          (my-choose* base
                      (base-expr-grammar desired-type depth)
                      (vector-ref-expr desired-type depth #:mutable? mutable?)))))

  ;; vector-ref
  (define (vector-ref-expr desired-type depth #:mutable? [mutable? #f])
    ;; TODO: Force the output type to be the desired-type
    (let* ([vec (expr-grammar (Vector-type (Bottom-type) desired-type)
                              (- depth 1) #:mutable? mutable?)]
           [vec-type (infer-type vec)])
      (if (Error-type? vec-type)
          (lifted-error)
          (vector-ref^ vec
                       (expr-grammar (Vector-index-type vec-type)
                                     (- depth 1))))))

  ;; Comparisons, arithmetic, integer holes
  (define (base-expr-grammar desired-type depth)
    (let ([options '()])
      (when (is-supertype? desired-type (Boolean-type))
        (let (#;[b1 (expr-grammar (Boolean-type) (- depth 1))]
              #;[b2 (expr-grammar (Boolean-type) (- depth 1))]
              [i1 (expr-grammar (Integer-type) (- depth 1))]
              [i2 (expr-grammar (Integer-type) (- depth 1))]
              [e1 (expr-grammar (Any-type) (- depth 1))]
              [e2 (expr-grammar (Any-type) (- depth 1))])
          (set! options
                (append options
                        (list (=^ i1 i2)
                              (<^ i1 i2)
                              (equal?^ e1 e2)
                              #;(and b1 b2)
                              #;(or b1 b2)
                              #;(not b1))))))
      (when (is-supertype? desired-type (Integer-type))
        (let ([i1 (expr-grammar (Integer-type) (- depth 1))]
              [i2 (expr-grammar (Integer-type) (- depth 1))])
          (define-symbolic* hole integer?)
          (set! options
                (append options
                        (list hole
                              (+^ i1 i2)
                              (-^ i1 i2)
                              (*^ i1 i2))))))
      (apply my-choose* options)))

  (build-grammar terminal-info num-stmts depth num-temps guard-depth
                 expr-grammar stmt-grammar))

(define (build-grammar terminal-info num-stmts depth num-temps guard-depth
                       expr-grammar stmt-grammar)
  ;; Choose guard
  (define guard-expr
    (and guard-depth (expr-grammar (Boolean-type) guard-depth)))
  
  ;; Add temporary variables for common subexpression reuse.
  ;; Generate temporary variable names
  (define temps
    (build-list num-temps
                (lambda (i) (gensym 'tmp))))

  ;; Choose definitions for each variable
  (define definitions
    (for/list ([sym temps])
      (let* ([subexp (expr-grammar (Any-type) 1)]
             [result (define-expr^ sym subexp)])
        (send terminal-info make-and-add-terminal sym (eval-lifted subexp)
              (infer-type subexp) #:mutable? #f)
        result)))

  ;; Build the program
  (define result
    (apply begin^
           (append definitions
                   (let ([stmts (stmt-grammar num-stmts depth)])
                     (if (list? stmts) stmts (list stmts))))))

  ;; Add guards if required
  (when guard-depth
    (set! result
          (if^ guard-expr (void^) result)))
  result)

;;;;;;;;;;;;;;;
;; Terminals ;;
;;;;;;;;;;;;;;;

;; TODO: Switch from "list of flags" to each flag being its own
;; separate keyword. This is because different flags have to be
;; handled differently in the grammar. For now it works because we
;; only care about "mutable".
(define Terminal-Info%
  (class object%
    (super-new)

    (field [symbol->terminal (make-hash)])

    (define/public (make-and-add-terminal symbol value type #:mutable? [mutable? #f])
      (add-terminal (make-lifted-variable symbol type #:value value
                                          #:mutable? mutable?)))

    (define/public (add-terminal terminal)
      (define symbol (variable-symbol terminal))
      (when (hash-has-key? symbol->terminal symbol)
        (error (format "Terminal ~a is already present!~%" symbol)))
      (hash-set! symbol->terminal symbol terminal))

    (define/public (get-terminal-by-id id)
      (hash-ref symbol->terminal id))

    ;; Returns the terminals which are instances of subtypes of the argument
    ;; type, and which have the associated flags.
    (define/public (get-terminals #:type [type (Any-type)]
                                  #:mutable? [mutable? #f])
      (filter (lambda (terminal)
                (and (is-supertype? type (variable-type terminal))
                     (or (not mutable?)
                         (variable-mutable? terminal))))
              (hash-values symbol->terminal)))))


  ;; (define result (lifted-define var val))
  ;; (when info
  ;;   (send info add-terminal var (eval-lifted subexp) (infer-type subexp)
  ;;         #:mutable #f)
