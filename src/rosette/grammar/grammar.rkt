#lang rosette

(require racket/syntax)

(require "choice.rkt" "grammar-synthax-deep.rkt"
         "lifted-operators.rkt" "language.rkt"
         "../types.rkt" "../variable.rkt")

(provide grammar Terminal-Info% eval-lifted lifted-code)


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
(define (grammar terminal-info operators num-stmts depth
                 #:num-temps [num-temps 0]
                 #:guard-depth [guard-depth 0]
                 #:type [type (Void-type)]
                 #:version [version 'basic]
                 #:choice-version [choice-version 'basic])
  (define chooser (make-chooser choice-version))
  (define result
    (if (or (= num-stmts 0) (= depth 0))
        (void^)
        (match version
          ['basic
           (grammar-basic terminal-info operators num-stmts depth chooser
                          #:num-temps num-temps
                          #:guard-depth guard-depth
                          #:type type)]
          ['caching
           (grammar-general terminal-info operators num-stmts depth chooser
                            #:num-temps num-temps
                            #:guard-depth guard-depth
                            #:type type
                            #:cache? #t)]
          ['general
           (grammar-general terminal-info operators num-stmts depth chooser
                            #:num-temps num-temps
                            #:guard-depth guard-depth
                            #:type type
                            #:cache? #f)]
          ['synthax-deep
           (match (list num-stmts depth)
             ['(2 2) (grammar-synthax-deep22 terminal-info)]
             ['(3 3) (grammar-synthax-deep33 terminal-info)]
             ['(3 4) (grammar-synthax-deep34 terminal-info)]
             [_ (error
                 (format "No synthax grammar for ~a statements and ~a expression depth"
                         num-stmts depth))])]
          [_
           (error (format "Unknown grammar type: ~a" version))])))
  
  (unless (member version '(synthax-deep))
    (send chooser print-num-vars))
  result)
            
(define (grammar-general terminal-info operators num-stmts expr-depth chooser
                         #:num-temps [num-temps 0]
                         #:guard-depth [guard-depth 0]
                         #:type [start-type (Void-type)]
                         #:cache? cache?)
  
  (define (my-choose* . args)
    (send chooser choose* args #f))

  ;; We use different caches for lookup and insertion. This is because
  ;; for every new choice, we create a copy of the cache so far, used
  ;; for lookup. However, when we generate new programs, we want them
  ;; to be used in future lookups, so we insert them into the original
  ;; cache.
  (define (cached-grammar desired-type #:mutable? mutable? depth
                          lookup-cache insert-cache remove?)
    ;; This depends on an understanding of the internals of Rosette.
    ;; In particular, this uses lots of impure code (hash-set! for the
    ;; cache) inside of a for/all, which documentation disallows.
    (apply-on-symbolic-type
     desired-type
     (lambda (concrete-type)
       (let* ([key (cons concrete-type mutable?)]
              [cache-val-list (if cache? (hash-ref lookup-cache key '()) '())])
         (if (null? cache-val-list)
             ;; Cache miss (or not caching). Generate a new program:
             (let ([result (general-grammar concrete-type #:mutable? mutable? depth)])
               ;; Insert into the cache if we're caching. Insert at
               ;; the end so that the cache is ordered by ascending
               ;; creation times (that is, the first item is the one
               ;; that was created first).
               (when cache?
                 (hash-set! insert-cache key
                            (append (hash-ref insert-cache key '())
                                    (list result))))
               result)
             ;; Cache hit. Remove the value so it isn't used again if
             ;; the remove? flag is set.
             ;; Exception: If the value is #f, then leave it, since we
             ;; don't need to worry about reusing #f values.
             (let ([result (car cache-val-list)])
               (when (and remove? result)
                 (hash-set! lookup-cache key (cdr cache-val-list)))
               result))))))

  ;; We only want if to be used for statements, not expressions
  (define if-type
    (Procedure-type (list (Boolean-type) (Void-type) (Void-type))
                    (Void-type)))
  
  (define (make-subexp-if cache desired-type mutable? depth)
    (and (not mutable?)
         ;; Forces us to only use if at the top level
         (= depth expr-depth)
         (let ([result (make-subexp-helper if-type cache
                                           desired-type mutable? depth)])
           (and result (apply if^ result)))))
  
  (define (make-subexp-set! cache desired-type mutable? depth)
    (and (unify-types (Void-type) desired-type)
         (not mutable?)
         ;; We only need to get one item out of cache, so no need to
         ;; make a copy which we then mutate.
         (let ([variable
                (apply my-choose*
                       (send terminal-info get-terminals #:mutable? #t))])
           (and variable
                (let* ([type (infer-type variable)]
                       ;; TODO: This may be wrong. Consider, if num1
                       ;; is mutable and num2 is not:
                       ;; (set! num1 num2)
                       ;; (vector-set! num1 0 0)
                       ;; Need to formalize what the mutability
                       ;; analysis must guarantee. Should things
                       ;; marked not mutable always be equal? What if
                       ;; num1 and num2 were aliased to begin with?
                       [subexp (cached-grammar type #:mutable? #f (- depth 1)
                                               cache cache #f)])
                  (and subexp (set!^ variable subexp)))))))

  (define (make-subexp-proc proc cache desired-type mutable? depth)
    (define result
      (make-subexp-helper (variable-type proc) cache
                          desired-type mutable? depth))
    (and result (apply proc result)))
  
  (define (make-subexp-helper operator-type cache desired-type mutable? depth)
    (let ([domain-pairs (get-domain-given-range-with-mutability
                         operator-type desired-type mutable?)]
          [cache-copy (and cache? (hash-copy cache))]
          [mapping (make-type-map)])
      
      (define (get-or-make-subexp type-pair)
        (let* ([simple-type (replace-type-vars (car type-pair) mapping #t)]
               [mutable? (cdr type-pair)]
               [subexp (cached-grammar simple-type #:mutable? mutable? (- depth 1)
                                       cache-copy cache #t)])
          
          ;; The value of this unify can be seen in equal?^, where if
          ;; the first expression chosen is of type Int, this will
          ;; force the second expression to also have type Int
          (when subexp
            (unify (car type-pair) (infer-type subexp) mapping))
          subexp))

      (define (special-andmap fn lst)
        (if (null? lst)
            lst
            (let* ([first (fn (car lst))]
                   [rest (and first (special-andmap fn (cdr lst)))])
              (and rest (cons first rest)))))

      (and domain-pairs (special-andmap get-or-make-subexp domain-pairs))))

  ;; special-andmap above creates weird symbolic terms because of
  ;; the complicated recursion. An alternative below.
  ;; However, this loses the short-circuiting behavior of andmap, and
  ;; so there will be more calls to get-or-make-subexp.
  ;; (define result
  ;;   (and domain-pairs (map get-or-make-subexp domain-pairs)))
  ;; (and result (not (member #f result)) result)))
  
  (define (make-subexp operator cache desired-type mutable? depth)
    (cond [(not (special-form? operator))
           (make-subexp-proc operator cache desired-type mutable? depth)]
          [(equal? (special-form-name operator) 'if)
           (make-subexp-if cache desired-type mutable? depth)]
          [(equal? (special-form-name operator) 'set!)
           (make-subexp-set! cache desired-type mutable? depth)]
          [else
           (error (format "Unknown special form: ~a" operator))]))

  ;; Type can be symbolic. This may make things tricky.
  ;; TODO: Is this sound? There may be some programs that we should
  ;; generate but don't, specifically when the desired-type is
  ;; symbolic and so we have too much sharing.
  ;; TODO: Use for/all to guarantee that desired-type will be concrete.
  (define (general-grammar desired-type depth #:mutable? [mutable? #f])
    (define cache (and cache? (make-hash)))

    ;; Base case: Terminals
    (define terminals
      (send terminal-info get-terminals #:type desired-type
            #:mutable? mutable?))

    ;; Special case: Integer hole
    (define integer-hole
      (if (unify-types (Integer-type) desired-type)
          (list (let () (define-symbolic* hole integer?) hole))
          '()))

    ;; Recursive case
    (send chooser start-options)
    (define recurse
      (if (= depth 0)
          '()
          (filter
           identity
           (for/list ([op operators])
             (send chooser begin-next-option)
             (make-subexp op cache desired-type mutable? depth)))))
    (send chooser end-options)
    
    (define all-args (append terminals integer-hole recurse))
    (apply my-choose* all-args))

  (if (equal? start-type (Void-type))
      (build-grammar terminal-info num-stmts expr-depth num-temps guard-depth
                     general-grammar
                     (lambda (num-stmts depth)
                       (build-list num-stmts
                                   (lambda (i)
                                     (general-grammar (Void-type) depth)))))
      (general-grammar start-type expr-depth)))

(define (grammar-basic terminal-info operators num-stmts depth chooser
                       #:num-temps [num-temps 0]
                       #:guard-depth [guard-depth 0]
                       #:type [type (Void-type)])
  (unless (equal? type (Void-type))
    (error (format "Basic grammar does not handle type ~a" type)))

  (define-syntax-rule (my-choose* arg ...)
    (choose* chooser (lifted-error) arg ...))
  (define (my-choose*-fn . args)
    (apply choose*-fn chooser (lifted-error) args))
  
  (define (stmt-grammar num-stmts depth)
    (if (= num-stmts 0)
        (void^)
        (my-choose* (void^)
                    (begin^ (base-stmt-grammar num-stmts depth)
                            (stmt-grammar (- num-stmts 1) depth)))))

  ;; Vector statements, set statements, set!, and if
  (define (base-stmt-grammar num-stmts depth)
    (if (<= num-stmts 2)
        (my-choose* (vector-stmt-grammar depth)
                    (set-stmt-grammar depth)
                    (set!-stmt-grammar depth))
        (my-choose* (vector-stmt-grammar depth)
                    (set-stmt-grammar depth)
                    (set!-stmt-grammar depth)
                    (if^ (expr-grammar (Boolean-type) depth)
                         (stmt-grammar 1 depth)
                         (stmt-grammar 1 depth)))))

  ;; vector-set!, vector-increment!, vector-decrement!
  (define (vector-stmt-grammar depth)
    (let* ([vec (expr-grammar (Vector-type (Index-type) (Any-type))
                              (- depth 1) #:mutable? #t)]
           [vec-type (infer-type vec)])
      (if (Error-type? vec-type)
          (lifted-error)
          (let* ([index (expr-grammar (Vector-index-type vec-type) (- depth 1))]
                 [value (expr-grammar (Vector-output-type vec-type) (- depth 1))])
            (my-choose* ((my-choose* vector-increment!^ vector-decrement!^) vec index)
                        (vector-set!^ vec index value))))))

  ;; enum-set-add!, enum-set-remove!
  (define (set-stmt-grammar depth)
    (let* ([sett (expr-grammar (Set-type (Any-type)) (- depth 1) #:mutable? #t)]
           [sett-type (infer-type sett)])
      (if (Error-type? sett-type)
          (lifted-error)
          (let* ([elem (expr-grammar (Set-content-type sett-type) (- depth 1))])
            (if (lifted-error? elem)
                (lifted-error)
                ((my-choose* enum-set-add!^ enum-set-remove!^) sett elem))))))

  (define (set!-stmt-grammar depth)
    (let* ([variable
            (apply my-choose*-fn
                   (send terminal-info get-terminals #:mutable? #t))])
      ;; TODO: Incorrect use of mutability, see earlier comment
      (set!^ variable (expr-grammar (infer-type variable) depth)))) 

  ;; If #:mutable is #t, then the return value must be mutable.
  ;; If #:mutable is #f, then the return value may or may not be mutable.
  (define (expr-grammar desired-type depth #:mutable? [mutable? #f])
    (let ([base (send terminal-info get-terminals #:type desired-type
                             #:mutable? mutable?)])
      (cond [(= depth 0)
             (apply my-choose*-fn base)]
            [mutable?
             (my-choose* (apply my-choose*-fn base)
                         (vector-ref-expr desired-type depth #:mutable? mutable?))]
            [else
             (my-choose* (apply my-choose*-fn base)
                         (base-expr-grammar desired-type depth)
                         (vector-ref-expr desired-type depth #:mutable? mutable?)
                         (enum-set-contains-expr desired-type depth))])))

  ;; vector-ref
  (define (vector-ref-expr desired-type depth #:mutable? [mutable? #f])
    (let* ([vec (expr-grammar (Vector-type (Index-type) desired-type)
                              (- depth 1) #:mutable? mutable?)]
           [vec-type (infer-type vec)])
      (if (Error-type? vec-type)
          (lifted-error)
          (vector-ref^ vec
                       (expr-grammar (Vector-index-type vec-type)
                                     (- depth 1))))))
  
  ;; enum-set-contains?
  (define (enum-set-contains-expr desired-type depth)
    (if (not (unify-types (Boolean-type) desired-type))
        (lifted-error)
        (let* ([sett (expr-grammar (Set-type (Any-type)) (- depth 1))]
               [sett-type (infer-type sett)])
          (if (Error-type? sett-type)
              (lifted-error)
              (let* ([elem (expr-grammar (Set-content-type sett-type) (- depth 1))])
                (if (lifted-error? elem)
                    (lifted-error)
                    (enum-set-contains?^ sett elem)))))))

  ;; Comparisons, arithmetic, integer holes
  (define (base-expr-grammar desired-type depth)
    (send chooser start-options)
    (let ([options '()])
      (when (unify-types (Boolean-type) desired-type)
        (let* (#;[_ (send chooser begin-next-option)]
               #;[b1 (expr-grammar (Boolean-type) (- depth 1))]
               #;[b2 (expr-grammar (Boolean-type) (- depth 1))]
               [_ (send chooser begin-next-option)]
               [i1 (expr-grammar (Integer-type) (- depth 1))]
               [i2 (expr-grammar (Integer-type) (- depth 1))]
               [_ (send chooser begin-next-option)]
               [e1 (expr-grammar (Any-type) (- depth 1))]
               [e2 (expr-grammar (Any-type) (- depth 1))])
          (set! options
                (append options
                        (list ((my-choose*-fn =^ <^) i1 i2)
                              (equal?^ e1 e2)
                              #;(and b1 b2)
                              #;(or b1 b2)
                              #;(not b1))))))
      (when (unify-types (Integer-type) desired-type)
        (let ([_ (send chooser begin-next-option)]
              [i1 (expr-grammar (Integer-type) (- depth 1))]
              [i2 (expr-grammar (Integer-type) (- depth 1))])
          (define-symbolic* hole integer?)
          (set! options
                (append options
                        (list hole
                              ((my-choose*-fn +^ -^ *^) i1 i2))))))
      (send chooser end-options)
      (apply my-choose*-fn options)))

  (build-grammar terminal-info num-stmts depth num-temps guard-depth
                 expr-grammar stmt-grammar))

(define (build-grammar terminal-info num-stmts depth num-temps guard-depth
                       expr-grammar stmt-grammar)
  ;; Choose guard
  (define guard-expr
    (and (> guard-depth 0) (expr-grammar (Boolean-type) guard-depth)))
  
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
  (when (> guard-depth 0)
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

    (define/public (has-terminal? id)
      (hash-has-key? symbol->terminal id))

    (define/public (get-terminal-by-id id)
      (hash-ref symbol->terminal id))

    ;; Returns the terminals which are instances of subtypes of the argument
    ;; type, and which have the associated flags.
    (define/public (get-terminals #:type [type (Any-type)]
                                  #:mutable? [mutable? #f])
      (filter (lambda (terminal)
                (and (unify-types type (variable-type terminal))
                     (or (not mutable?)
                         (variable-mutable? terminal))))
              (hash-values symbol->terminal)))))


  ;; (define result (lifted-define var val))
  ;; (when info
  ;;   (send info add-terminal var (eval-lifted subexp) (infer-type subexp)
  ;;         #:mutable #f)
