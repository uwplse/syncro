#lang rosette

(require racket/syntax racket/generator)

(require "choice.rkt" "grammar-operators.rkt"
         "lifted-operators.rkt" "language.rkt"
         "../types.rkt" "../variable.rkt" "../util.rkt")

(provide grammar Terminal-Info% eval-lifted lifted-code eliminate-dead-code
         ;; For testing
         remove-polymorphism)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grammar construction ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; terminal-info: A Terminal-Info object
;; num-stmts:     Number of statements to allow
;; depth:         Expression depth to allow
;; #:num-temps:   Number of temporary variables to add to the sketch
;; #:guard-depth: Depth of the guard expression. If this is #f, no
;;                guard is inserted.
;; #:type:        The output type of the expression to be generated.
;; #:operators:   A list of operators to use. Each operator is either
;;                a grammar-operator? or a symbol (like 'set!)
;; #:version and #:choice-version are settings determining which
;; grammar to use and how to use it.
;; Note that the num-stmts and depth do not have exact meanings, they
;; are simply used as costs. (For example, despite an if having
;; multiple statements inside it, it counts as only one statement.)
(define (grammar terminal-info num-stmts depth
                 #:num-temps [num-temps 0]
                 #:guard-depth [guard-depth 0]
                 #:type [type (Void-type)]
                 #:operators [operator-info default-operators]
                 #:version [version 'basic]
                 #:choice-version [choice-version 'basic])
  (define-values (new-pairs-set all-pairs-set operators)
    (remove-polymorphism operator-info terminal-info))
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
                            #:use-constants? #t
                            #:type type
                            #:cache? #t)]
          ['general
           (grammar-general terminal-info operators num-stmts depth chooser
                            #:num-temps num-temps
                            #:guard-depth guard-depth
                            #:use-constants? #t
                            #:type type
                            #:cache? #f)]
          [`(ssa ,(? integer? num-constants))
           (grammar-ssa terminal-info operators num-stmts depth chooser
                        #:num-temps num-temps
                        #:guard-depth guard-depth
                        #:num-constants num-constants
                        #:type type
                        #:new-pairs-set new-pairs-set
                        #:cache? #t)]
          #;['synthax-deep
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


(define (grammar-ssa terminal-info operators num-stmts expr-depth chooser
                     #:num-temps [num-temps 0]
                     #:guard-depth [guard-depth 0]
                     #:num-constants [num-constants 0]
                     #:type [start-type (Void-type)]
                     #:new-pairs-set new-pairs-set
                     #:cache? cache?)
  (define (generate type mutable? stmts depth)
    (grammar-general terminal-info operators stmts depth chooser
                     #:num-temps 0 #:guard-depth 0
                     #:use-constants? #f #:cache? cache?
                     #:type type #:mutable? mutable?))

  ;; pair: tm-pair defining what type of value we want and whether or
  ;; not it should be mutable.
  (define (make-temporary pair)
    (match pair
      [(tm-pair type mutable?)
       (define sym (gensym 'tmp))
       (define subexp (generate type mutable? 1 1))
       ;; Don't add the terminal if it is definitely #f, but add it
       ;; if it may not be #f
       (define lifted-sym
         (if (and (not (term? subexp)) (false? subexp))
             (send terminal-info make-and-add-terminal sym #f type
                   #:mutable? mutable?)
             (make-lifted-variable sym (Void-type) #:value (void^)
                                   #:mutable? #f)))
       (define^ lifted-sym subexp)]))

  (define holes
    (for/list ([i num-constants])
      (define-symbolic* hole integer?)
      (define sym (gensym 'constant))
      (define lifted-sym
        (send terminal-info make-and-add-terminal sym hole
              (Integer-type) #:mutable? #f))
      (define^ lifted-sym hole)))

  ;; Choose definitions for each variable
  ;; We only consider new-pairs-set here, which is the set of all
  ;; tm-pairs that can be generated by applying operators -- in
  ;; particular, it does not include tm-pairs that can only be
  ;; obtained by choosing variables, because we don't want to have
  ;; temporary variables that are the same as variables.
  ;; Loop order is important here. If we have types A and B, we want
  ;; to generate A B A B A B instead of A A A B B B, so that any
  ;; expressions that produce A that require B can be synthesized.
  ;; Dead code elimination assumes that any definitions here do not
  ;; modify state, so we don't allow Void type. For now we assume
  ;; that all procedures that modify state return void.
  ;; TODO: Better solution to the state modification problem.
  (define definitions
    (for*/list ([i expr-depth]
                [tmp-pair new-pairs-set]
                #:unless (equal? (tm-pair-type tmp-pair) (Void-type)))
      (make-temporary tmp-pair)))

  ;; Build the program
  (apply begin^
         (append holes definitions
                 (list (generate (Void-type) #f num-stmts 2)))))

(define (grammar-general terminal-info operators num-stmts expr-depth chooser
                         #:num-temps [num-temps 0]
                         #:guard-depth [guard-depth 0]
                         #:use-constants? [use-constants? #t]
                         #:type [start-type (Void-type)]
                         #:mutable? [mutable? #f]
                         #:cache? cache?)
  (define orig-params
    (hash 'terminal-info terminal-info
          'operators operators
          'num-stmts num-stmts
          'expr-depth expr-depth
          'chooser chooser
          'num-temps num-temps
          'guard-depth guard-depth
          'type start-type
          'mutable? mutable?
          'cache? cache?))

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
       (let* ([key (tm-pair concrete-type mutable?)]
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
  
  (define (make-subexp-operator operator cache desired-type mutable? depth)
    (let ([domain-pairs
           (and (can-use-operator? operator orig-params depth desired-type)
                (operator-domain-with-mutability operator desired-type mutable?))]
          [cache-copy (and cache? (hash-copy cache))]
          [mapping (make-type-map)])
      
      (define (get-or-make-subexp pair)
        (let* ([simple-type (replace-type-vars (tm-pair-type pair) mapping #t)]
               [mutable? (tm-pair-mutable? pair)]
               [subexp (cached-grammar simple-type #:mutable? mutable? (- depth 1)
                                       cache-copy cache #t)])
          
          ;; The value of this unify can be seen in equal?^, where if
          ;; the first expression chosen is of type Int, this will
          ;; force the second expression to also have type Int
          #;(when subexp
            (unify (car type-pair) (infer-type subexp) mapping))
          subexp))

      (define (special-andmap fn lst)
        (if (null? lst)
            lst
            (let* ([first (fn (car lst))]
                   [rest (and first (special-andmap fn (cdr lst)))])
              (and rest (cons first rest)))))

      (let* ([result (and domain-pairs
                          (special-andmap get-or-make-subexp domain-pairs))])
        (and result (operator-make-lifted operator result)))))

  ;; special-andmap above creates weird symbolic terms because of
  ;; the complicated recursion. An alternative below.
  ;; However, this loses the short-circuiting behavior of andmap, and
  ;; so there will be more calls to get-or-make-subexp.
  ;; (define result
  ;;   (and domain-pairs (map get-or-make-subexp domain-pairs)))
  ;; (and result (not (member #f result)) result)))
  (define special-form->proc
    (hash 'set! make-subexp-set!))
  
  (define (make-subexp operator cache desired-type mutable? depth)
    (cond [(grammar-operator? operator)
           (make-subexp-operator operator cache desired-type mutable? depth)]
          [(hash-has-key? special-form->proc operator)
           (define proc (hash-ref special-form->proc operator))
           (proc cache desired-type mutable? depth)]
          [else
           (error (format "Unknown special form: ~a" operator))]))

  ;; Type can be symbolic. This may make things tricky.
  ;; TODO: Is this sound? There may be some programs that we should
  ;; generate but don't, specifically when the desired-type is
  ;; symbolic and so we have too much sharing.
  (define (general-grammar desired-type depth #:mutable? [mutable? #f])
    (define cache (and cache? (make-hash)))

    ;; Base case: Terminals
    (define terminals
      (send terminal-info get-terminals #:type desired-type
            #:mutable? mutable?))

    ;; Special case: Integer hole
    (define integer-hole
      (if (and use-constants? (unify-types (Integer-type) desired-type))
          (list (let () (define-symbolic* hole integer?) hole))
          '()))

    (define boolean-constants
      (if (unify-types (Boolean-type) desired-type)
          (list #t #f)
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
    
    (define all-args (append terminals integer-hole boolean-constants recurse))
    (apply my-choose* all-args))

  (if (equal? start-type (Void-type))
      (build-grammar terminal-info num-stmts expr-depth num-temps guard-depth
                     general-grammar
                     (lambda (num-stmts depth)
                       (build-list num-stmts
                                   (lambda (i)
                                     (general-grammar (Void-type) depth)))))
      (general-grammar start-type expr-depth #:mutable? mutable?)))

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
      (define subexp (expr-grammar (Any-type) 1))
      (define lifted-sym
        (send terminal-info make-and-add-terminal sym (eval-lifted subexp)
              (infer-type subexp) #:mutable? #f))
      (define^ lifted-sym subexp)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove polymorphism ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Would not notice bound variables in a procedure type.
(define (check-no-polymorphism type msg)
  (unless (null? (get-free-type-vars type))
    (error (format "Type ~a should not have type variables! ~a" type msg))))

;; Polymorphism during grammar generation is really annoying because
;; it means you need to have a symbolic unification algorithm. Rather
;; than doing that, we infer all possible concrete types that could
;; come up during grammar generation, and use those instead, so that
;; there is no polymorphism left.
;; This depends on a crucial property -- no lifted operator creates a
;; "bigger" type than its arguments, where the size of a type is the
;; number of type constructors called to make that type. Since there
;; are a finite number of types of size <= k for any constant k, this
;; must terminate.
;; This also has the nice property of removing any operators that
;; cannot be applied in the grammar since there is no way of using
;; them. (eg. In a program using only vector types, we'd remove
;; enum-set functions.)
;; TODO: Also look at mutability, for additional filtering. Not needed
;; for correctness but would help performance.
(define (remove-polymorphism operators terminal-info)
  ;; Split into two phases -- first, construct all types that could
  ;; possibly arise, and second, specialize procedure types in all
  ;; the ways possible.

  (define init-pairs-list
    ;; Integer holes can always create integers
    (cons (tm-pair (Integer-type) #f)
          (map (lambda (v) (tm-pair (variable-type v) (variable-mutable? v)))
               (send terminal-info all-terminals))))

  (define init-pairs (for/set ([p init-pairs-list]) p))

  ;; Assumption: All of the types in init-pairs do not have any type
  ;; variables in them.
  (for ([pair init-pairs])
    (check-no-polymorphism (tm-pair-type pair)
                           "Terminal types should not be polymorphic."))

  (let* ([one-iter-pairs (get-new-types operators init-pairs)]
         [new-pairs (get-all-new-types operators init-pairs one-iter-pairs)]
         [all-pairs (set-union init-pairs new-pairs)]
         [new-ops (specialize-operators operators all-pairs)])
    (values new-pairs all-pairs new-ops)))

;; Returns a set of tm-pairs p such that a value with type/mutable p
;; can be constructed from some operator in operators applied to
;; values with some tm-pair p' in pairs.
;; operators: Sequence of operators, as accepted by the grammar
;; pairs: Set of tm-pairs
(define (get-new-types operators pairs)
  (for*/set ([op operators]
             [(_ range-pair) (try-apply-op op pairs)])
    range-pair))

;; Returns the set of tm-pairs p such that a value with type/mutable p
;; can be constructed by applying some non-empty sequence of operators
;; to values with tm-pairs p' in pairs.
;; Note that the non-empty qualifier means that some pairs in
;; init-pairs may NOT be present in the output!
;; operators: Sequence of operators, as accepted by the grammar
;; pairs: Set of tm-pairs
(define (get-all-new-types operators init-pairs new-pairs)
  (let loop ([new-pairs new-pairs])
    (define updated-new-pairs
      (get-new-types operators (set-union init-pairs new-pairs)))
    (if (subset? updated-new-pairs new-pairs)
        new-pairs
        (loop (set-union new-pairs updated-new-pairs)))))

;; Specializes operators. See try-apply-op for details.
(define (specialize-operators operators pairs)
  (set->list
   (for*/set ([op operators]
              [(new-op _) (try-apply-op op pairs)])
     new-op)))

;; op: Lifted operator whose type is a Procedure-type.
;; types: A sequence of non-polymorphic types.
;; Returns a sequence (that wraps a generator).
;; Each element of the sequence consists of two values.
;; The first value is a new operator that corresponds to a possible
;; application of op to values whose types are in types. In
;; particular, the new operator must *not* use unification with type
;; variables in its operator-domain-with-mutability implementation.
;; For procedures, this means that the type is a Procedure-Type that
;; does not contain any type variables (free or bound).
;; The second element of the sequence is the tm-pair that would be
;; returned by the application of op.
;; This sequence could contain duplicates.
;; Stupid algorithm: Try all combinations of types. Can definitely
;; improve this if necessary -- if we use functional type maps, then
;; we can do a recursive procedure where we match each element of the
;; domain at a time and only go on if it succeeds. Still worst case
;; O(n^d) where d is (length domain), but *much* more filtering that
;; makes it a lot better in normal cases.
(define (try-apply-op op pairs)
  (define (n-product seq n)
    (if (= n 0)
        (in-generator (yield '()))
        (in-generator
         (let ([subseq (n-product seq (- n 1))])
           (for* ([sublst subseq]
                  [elem seq])
             (yield (cons elem sublst)))))))

  (define (try-apply-proc op pairs)
    (let* ([proc-type (variable-type op)]
           [domain (Procedure-domain-types proc-type)]
           [range (Procedure-range-type proc-type)]
           [ridx (Procedure-read-index proc-type)]
           [widx (Procedure-write-index proc-type)])
      (in-generator
       #:arity 2
       (for ([possible-domain (n-product pairs (length domain))])
         (define new-range-pair
           (apply-type-with-mutability proc-type possible-domain))
         (when new-range-pair
           (let* ([new-domain (map tm-pair-type possible-domain)]
                  [new-range-type (tm-pair-type new-range-pair)]
                  [new-proc-type
                   (Procedure-type new-domain new-range-type
                                   #:read-index ridx #:write-index widx)])
             ;; How do we know at this point that polymorphism is gone?
             ;; The types in types do not have polymorphism, and we
             ;; assume that applying a procedure to non-polymorphic
             ;; types results in a non-polymorphic type (it wouldn't
             ;; make sense to introduce a type variable in the range).
             (check-no-polymorphism new-proc-type "Procedures should not introduce polymorphism in the range.")
             (yield (update-lifted-variable op #:type new-proc-type)
                    new-range-pair)))))))

  (define (try-apply-get-field pairs)
    (in-generator
     #:arity 2
     (for ([record-pair pairs]
           #:when (Record-type? (tm-pair-type record-pair)))
       (match record-pair
         [(tm-pair rec-type rec-mutable?)
          (for ([field-name (Record-fields rec-type)]
                [field-type (Record-field-types rec-type)]
                [constant-field? (Record-field-constant? rec-type)])
            (check-no-polymorphism rec-type "Error: Should be impossible.")
            (check-no-polymorphism field-type "Error: Should be impossible.")
            (yield
             (make-operator
              'get-field
              (Procedure-type (list rec-type) field-type #:read-index 0)
              (lambda (rec) (get-field^ rec field-name)))
             ;; Result is mutable if the record is mutable and the field
             ;; is not const
             (tm-pair field-type (and rec-mutable? (not constant-field?)))))]))))

  (define (try-apply-set-field! types)
    (in-generator
     #:arity 2
     (for ([record-pair pairs]
           #:when (Record-type? (tm-pair-type record-pair)))
       (match record-pair
         [(tm-pair rec-type rec-mutable?)
          (for ([field-name (Record-fields rec-type)]
                [field-type (Record-field-types rec-type)]
                [constant-field? (Record-field-constant? rec-type)])
            (check-no-polymorphism rec-type "Error: Should be impossible.")
            (check-no-polymorphism field-type "Error: Should be impossible.")
         (unless constant-field?
           (yield
            (make-operator
             'set-field!
             (Procedure-type (list rec-type field-type) (Void-type)
                             #:write-index 0)
             (lambda (rec val) (set-field!^ rec field-name val)))
            (tm-pair (Void-type) #f))))]))))

  (if (lifted-variable? op)
      (try-apply-proc op pairs)
      (match (if (grammar-operator? op) (operator-id op) op)
        ['get-field (try-apply-get-field pairs)]
        ['set-field! (try-apply-set-field! pairs)]
        ['if (in-generator #:arity 2
                           (yield op (tm-pair (Void-type) #f)))]
        ['set! (in-generator #:arity 2
                             (yield op (tm-pair (Void-type) #f)))]
        [_ (error (format "try-apply-op: Unknown operator -- ~a" op))])))

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

    (define/public (make-and-add-terminal sym value type
                                          #:mutable? [mutable? #f])
      (define terminal
        (make-lifted-variable sym type #:value value #:mutable? mutable?))
      (add-terminal terminal)
      terminal)

    (define/public (add-terminal terminal)
      (define symbol (variable-symbol terminal))
      (when (hash-has-key? symbol->terminal symbol)
        (error (format "Terminal ~a is already present!~%" symbol)))
      (hash-set! symbol->terminal symbol terminal))

    (define/public (all-terminals)
      (hash-values symbol->terminal))

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
