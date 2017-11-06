#lang rosette

(require racket/syntax racket/generator)

(require "choice.rkt" "grammar-operators.rkt"
         "lifted-operators.rkt" "language.rkt"
         "../types.rkt" "../variable.rkt" "../util.rkt")

(provide grammar Lexical-Terminal-Info%
         eval-lifted lifted-code eliminate-dead-code
         ;; For testing
         remove-polymorphism)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grammar construction ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Generates a symbolic program (that is, a set of programs
;; represented as a choice over concrete programs through Rosette
;; boolean variables) that when evaluated would produce a value of the
;; given type. Variables in the symbolic program are drawn from
;; terminal-info, functions and special forms are drawn from
;; operators, and the size of the programs generated is constrained by
;; num-stmts and depth. A common use case is to ask for (Void-type),
;; in which case we care not about the return value (which is always
;; #<void>) but the side effects that occur along the way.
;; Note that most of the parameters have no effect on the basic
;; grammar, which hardcodes many things (such as the list of
;; operators).
;; Note that the num-stmts and depth do not have exact meanings, they
;; are simply used as costs. (For example, despite an if having
;; multiple statements inside it, it counts as only one statement.)
;; terminal-info: A Lexical-Terminal-Info% object
;; num-stmts:     Number of statements to allow
;; depth:         Expression depth to allow
;; #:num-temps:   Number of temporary variables to add to the sketch
;; #:guard-depth: Depth of the guard expression. If this is #f, no
;;                guard is inserted.
;; #:type:        The output type of the expression to be generated.
;; #:disable-types?: Whether to disable the type analysis. Does not
;;                affect the basic grammar.
;; #:operators:   A list of operators to use. Each operator is either
;;                a grammar-operator? or a symbol (like 'set!)
;; #:version:     Which grammar to use (basic, general, caching, ssa)
;; #:choice-version: Which choice strategy to use (basic or sharing).
;; #:mode:        Whether to generate a statement or an expression.
;; #:print-statistics: #t if we should print the number of boolean
;;                     variables used to encode the grammar, #f
;;                     otherwise.
;; Returns: A symbolic lifted? object representing the set of
;;          type-safe and mutability-safe programs of size limited in
;;          some way by depth/num-stmts that when evaluated produce a
;;          value of the given type. If mutable? is #t and we are not
;;          using the basic grammar, then the values returned by the
;;          programs must be allowed to be mutated according to the
;;          mutability analysis. Returns #f if no program satisfying
;;          the constraints exists. Note that #f is lifted? and so it
;;          is always possible to call eval-lifted, lifted-code
;;          etc. on the result of a call to grammar.
(define (grammar terminal-info num-stmts depth
                 #:num-temps [num-temps 0]
                 #:guard-depth [guard-depth 0]
                 #:type [type (Void-type)]
                 #:disable-types? [disable-types? #f]
                 #:mutable? [mutable? #f]
                 #:operators [operator-info default-operators]
                 #:version [version 'basic]
                 #:choice-version [choice-version 'basic]
                 #:cache? [cache? #t]
                 #:mode [mode 'stmt]
                 #:print-statistics [print-statistics #f])
  (when disable-types? (set! type (Any-type)))
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
          ['general
           (grammar-general terminal-info operators num-stmts depth chooser
                            #:num-temps num-temps
                            #:guard-depth guard-depth
                            #:use-constants? #t
                            #:type type
                            #:mutable? mutable?
                            #:cache? cache?
                            #:disable-types? disable-types?
                            #:mode mode)]
          [`(ssa ,(? integer? num-constants))
           (grammar-ssa terminal-info operators num-stmts depth chooser
                        #:num-temps num-temps
                        #:guard-depth guard-depth
                        #:num-constants num-constants
                        #:type type
                        #:mutable? mutable?
                        #:new-pairs-set new-pairs-set
                        #:cache? cache?
                        #:disable-types? disable-types?
                        #:mode mode)]
          [_
           (error (format "Unknown grammar type: ~a" version))])))
  
  (when print-statistics (send chooser print-stats))
  result)

;; Creates a symbolic program that defines a new temporary variable.
;; TODO: The meaning of mutable? here seems different from all the
;; other places we use mutable?, is this a bug?
;; Note that there is an important invariant that callers must ensure
;; holds true between fn and type + mutable?, detailed in the
;; documentation for fn.
;; terminal-info: Lexical-Terminal-Info% object, may be mutated
;; type: A Type?, the type that the generated expression should
;;       evaluate to
;; mutable?: Boolean, #t if the generated temporary variable is
;;           allowed to be mutated in the future
;; fn: Function of no arguments that returns a lifted? value or
;;     #f. If not #f, the returned value is a subtree satisfying the
;;     type and mutability constraints implied by type and mutable?.
;; Returns: A symbolic program of the form (define tmp (??)) where tmp
;;          is a fresh temporary variable and (??) is either #f or a
;;          subtree satisfying the type and mutability constraints.
(define (create-temporary terminal-info type mutable? fn)
  (define sym (gensym 'tmp))
  (define subexp (fn))
  ;; Don't add the terminal if it is definitely #f, but add it
  ;; if it may not be #f
  (define lifted-sym
    (if (and (not (symbolic? subexp)) (false? subexp))
        (make-lifted-variable sym (Void-type) #:mutable? #f)
        (send terminal-info make-and-add-terminal sym type
              #:mutable? mutable?)))
  (define^ lifted-sym subexp))

;; Same interface as grammar, defined above.
;; Generates a program of the following form:
;; (let ()
;;   (define constant1 (??))  ;; A symbolic integer
;;   (define constant2 (??))
;;   ...
;;   (define tmp1 (??))  ;; A boolean hole, perhaps
;;   (define tmp2 (??))  ;; A vector hole, perhaps
;;   ...
;;   (let ()
;;     (??)  ;; statement that mutates something
;;     ...))
;; This uses the general grammar to generate a lot of small, shallow
;; expressions or statements, and then stitches them together, whereas
;; the general grammar with large parameters would generate one
;; monolithic AST.
;; chooser: An object of one of the Chooser% classes in choice.rkt.
;; new-pairs-set: TODO: Describe
(define (grammar-ssa terminal-info operators num-stmts expr-depth chooser
                     #:num-temps [num-temps 0]
                     #:guard-depth [guard-depth 0]
                     #:num-constants [num-constants 0]
                     #:type [start-type (Void-type)]
                     #:mutable? [mutable? #f]
                     #:new-pairs-set new-pairs-set
                     #:cache? cache?
                     #:disable-types? [disable-types? #f]
                     #:mode mode)
  (when mutable?
    (error "Unsupported grammar option: SSA grammar does not enforce top level mutability constraint"))

  ;; Does the same thing as grammar-general, but provides default
  ;; values for many of the options. This is optimized to simply
  ;; generate a normal AST without any bells or whistles.
  (define (generate type mutable? stmts depth)
    (grammar-general terminal-info operators stmts depth chooser
                     #:num-temps 0 #:guard-depth 0
                     #:use-constants? #f #:cache? cache?
                     #:disable-types? disable-types?
                     #:type type #:mutable? mutable?
                     #:mode 'stmt))

  ;; Wrapper around create-temporary to pass in some default
  ;; parameters (such as the terminal-info).
  (define (create-ssa-temporary type-mutability-pair)
    (match type-mutability-pair
      [(tm-pair type mutable?)
       (create-temporary terminal-info type mutable?
                         (lambda () (generate type mutable? 1 1)))]))

  ;; List of lifted-define objects, each of which defines a symbolic
  ;; integer constant.
  (define integer-holes
    (for/list ([i num-constants])
      (define-symbolic* hole integer?)
      (define sym (gensym 'constant))
      (define lifted-sym
        (send terminal-info make-and-add-terminal sym (Integer-type)
              #:mutable? #f))
      (define^ lifted-sym hole)))

  ;; Choose definitions for each variable, producing a list of
  ;; lifted-define objects.
  ;; We only consider new-pairs-set here, which is the set of all
  ;; tm-pairs that can be generated by applying operators -- in
  ;; particular, it does not include tm-pairs that can only be
  ;; obtained by choosing variables, because we don't want to have
  ;; temporary variables that are the same as existing variables.
  ;; Loop order is important here. If we have types A and B, we want
  ;; to generate A B A B A B instead of A A A B B B, so that any
  ;; expressions that produce A that require B can be synthesized.
  ;; Dead code elimination assumes that any definitions here do not
  ;; modify state, so we don't allow Void type. For now we assume
  ;; that all procedures that modify state return void.
  ;; TODO: Better solution to the problem above.
  (define num-defns
    (if (equal? mode 'stmt)
        expr-depth
        (* expr-depth (second mode))))
  (define definitions
    (if disable-types?
        (for/list ([i num-defns])
          (create-ssa-temporary (tm-pair (Any-type) #t)))
        (for*/list ([i num-defns]
                    [tmp-pair new-pairs-set]
                    #:unless (equal? (tm-pair-type tmp-pair) (Void-type)))
          (create-ssa-temporary tmp-pair))))

  ;; Build the program
  (if (equal? mode 'stmt)
      (apply begin^
             (append integer-holes definitions
                     (list (generate (Void-type) #f num-stmts 2))))
      (append integer-holes definitions)))


;; Similar interface as grammar (defined above).
;; This is the main workhorse of grammar generation. In particular, it
;; makes sure that all generated programs are type safe.
;; Optionally, the grammar can also apply a caching optimization in
;; order to share subtrees among branches where it is safe to do so.
(define (grammar-general terminal-info operators num-stmts expr-depth chooser
                         #:num-temps [num-temps 0]
                         #:guard-depth [guard-depth 0]
                         #:use-constants? [use-constants? #t]
                         #:type [start-type (Void-type)]
                         #:mutable? [mutable? #f]
                         #:cache? cache?
                         #:disable-types? [disable-types? #f]
                         #:mode mode)
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
          'cache? cache?
          'disable-types? disable-types?
          'mode mode))

  (define (my-choose* . args)
    (send chooser choose* args #f))

  ;; Caching is tricky to get right. Consider the following symbolic
  ;; AST:
  ;;                        (?? int)
  ;;            /----------/   |    \--\
  ;;       vector-ref          +        -
  ;;        /      \          / \      / \
  ;; (?? vector) (?? int)

  ;; Here, we are about to generate two subtrees of the form (?? int)
  ;; for the +, and then we'll go on to the -. Note that these two
  ;; subtrees cannot be shared -- they must use different symbolic
  ;; booleans at choice nodes, otherwise they would be forced to be
  ;; the same program, and you would be forced to add a number to
  ;; itself (basically the + is acting more like (curry * 2)). So we
  ;; do need two different subtrees. However, since there is a choice
  ;; node above that lets us pick between vector-ref and +, there will
  ;; never be a program that has *both* the vector-ref and the +, and
  ;; so it is safe to reuse the (?? int) subtree from vector-ref
  ;; (let's call that tree T1). But T1 can only be reused as *one* of
  ;; the subtrees for +, the other subtree T2 must be generated from
  ;; scratch with new symbolic boolean choice nodes.
  ;; Now, once we've done that, we can move on to the -. Now, since
  ;; we're once again under a choice node and we can never conflict
  ;; with the vector-ref or the +, we can actually reuse both T1 *and*
  ;; T2, and so we don't need to generate anything new this time.

  ;; This means that we have an unusual caching policy, where every
  ;; time there's a cache hit, we can use that subtree, but then we
  ;; need to delete it while filling out the rest of the arguments,
  ;; but then once we backtrack to the choice node and start on
  ;; another subtree, we can insert it back into the cache.

  ;; In order to actually implement such an algorithm, we use both a
  ;; temporary lookup cache and a less temporary true cache. Every
  ;; time we start a new choice node, we create a new true
  ;; cache. (This part happens in grammar-general-helper, the new
  ;; cache is then passed in to cached-grammar.) Every time we start
  ;; a new choice within a choice node, the lookup cache is reset to
  ;; be a copy of the true cache, since all of the subtrees that were
  ;; used and deleted from the lookup cache are now eligible to be
  ;; reused again. Finally, every time we attempt to generate a new
  ;; symbolic program, we check the cache to see if there's an
  ;; eligible subtree. For a cache hit, we reuse the subtree and
  ;; delete it from the lookup cache (but *not* the true cache). For a
  ;; cache miss, we generate a new subtree from scratch, use it, and
  ;; add it to the true cache (but *not* the lookup cache, since we
  ;; just used it).

  ;; The parts of this algorithm which involve creating new versions
  ;; of the true cache and lookup cache must be implemented by callers
  ;; of cached-grammar. cached-grammar assumes that the caches are
  ;; maintained in this way, and then performs cache lookups and deals
  ;; with cache hits and cache misses appropriately.

  ;; The keys to the caches are tm-pairs (which, combined with depth,
  ;; uniquely identify the subtree that should be generated). The
  ;; depth is not included in the key because any cache instance will
  ;; only contain subtrees of a specific depth. The values are *lists*
  ;; of subtrees, because it is possible to generate multiple subtrees
  ;; for the same key. In the example above, the key (tm-pair int #f)
  ;; would have the value (list T1 T2) at the point where it is about
  ;; to generate subtrees for -. Once we reuse the subtree T1 for the
  ;; first argument to -, the value would be updated to (list T2).

  ;; As if this were not tricky enough already, we sometimes can have
  ;; symbolic keys to the cache. One potential solution would be to
  ;; implement the cache as an associative list, which Rosette will
  ;; automatically lift to work on symbolic keys. However, this leads
  ;; to extremely expensive symbolic computation, so it doesn't
  ;; work. Usually though, symbolic keys only encode a small number of
  ;; concrete keys, and so another strategy is to take each symbolic
  ;; key, enumerate the set of possible concrete keys, perform the
  ;; algorithm above on each concrete key separately to get a set of
  ;; subtrees, and then merge the set of subtrees into a single
  ;; subtree by guarding each subtree with whatever boolean condition
  ;; guarded the corresponding concrete key. With this strategy, the
  ;; caches themselves only ever see concrete keys and as a result
  ;; they can be implemented as plain hash maps.
  ;; Note that by doing this we are taking all of the guarantees of
  ;; safe Rosette and stomping them into the ground. This is very much
  ;; dependent on a lot of knowledge about Rosette internals that is
  ;; not guaranteed by its API. In particular, it uses impure code
  ;; (hash-set! for the cache) inside of a for/all.

  ;; It is possible to disable the removal of a cache hit from the
  ;; lookup cache, by passing #f for remove?. This is safe when you
  ;; know that you are going to use the cache at most once, in which
  ;; case you can pass in the true cache for the lookup cache (instead
  ;; of making a copy) and disable removal on cache hits. This saves
  ;; you a hash copy (you don't need to create the lookup cache).
  ;; TODO: Remove this feature, it's premature optimization, far too
  ;; complicated for the meager benefit it provides.

  ;; desired-type, mutable?, depth: Parameters controlling the subtree
  ;; to produce. See grammar documentation.
  ;; lookup-cache: The lookup cache, described above.
  ;; true-cache:   The true cache, described above.
  ;; remove?:      Whether to remove a subtree found in a cache hit.
  ;; Returns: A subtree (lifted? object) satisfying the constraints
  ;;          implied by desired-type, mutable? and depth, or #f.
  (define (cached-grammar desired-type #:mutable? mutable? depth
                          lookup-cache true-cache remove?)
    (when disable-types? (set! desired-type (Any-type)))
    (apply-on-symbolic-type
     desired-type
     (lambda (concrete-type)
       (let* ([key (tm-pair concrete-type mutable?)]
              [cache-val-list (if cache? (hash-ref lookup-cache key '()) '())])
         (if (null? cache-val-list)
             ;; Cache miss (or not caching). Generate a new program:
             (let ([result (grammar-general-helper concrete-type #:mutable? mutable? depth)])
               ;; Insert into the true cache if we're caching. Insert
               ;; at the end so that the cache is ordered by ascending
               ;; creation times (that is, the first item is the one
               ;; that was created first). This means that if we
               ;; generate (+ T1 T2) and we then want to reuse
               ;; subtrees for -, we will get (- T1 T2) rather than
               ;; (- T2 T1), which is not as good for state merging.
               (when cache?
                 (hash-set! true-cache key
                            (append (hash-ref true-cache key '())
                                    (list result))))
               result)
             ;; Cache hit. Remove the value so it isn't used again if
             ;; the remove? flag is set.
             ;; Exception: If the value is not symbolic, then we can
             ;; reuse it as much as we want, so leave it in the
             ;; cache. In particular, values of #f (which mean that
             ;; no program can satisfy the tm-pair) will be reused.
             (let ([result (car cache-val-list)])
               (when (and remove? (symbolic? result))
                 (hash-set! lookup-cache key (cdr cache-val-list)))
               result))))))

  ;; Returns a lifted-set! object, or #f if no such program exists.
  ;; Note that since a set! always returns #<void>, if desired-type is
  ;; not compatible with (Void-type), this will return #f.
  ;; cache: The true cache. See description of caching algorithm above
  ;;        cached-grammar.
  (define (make-subexp-set! cache desired-type mutable? depth)
    (when disable-types? (set! desired-type (Any-type)))
    (and (unify-types (Void-type) desired-type)
         (not mutable?)
         ;; We only need to get one item out of cache, so no need to
         ;; make a copy which we then mutate. See also description of
         ;; the caching algorithm before cached-grammar.
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

  ;; Returns a lifted? object that applies the given operator to some
  ;; arguments. The return value must satisfy the constraints implied
  ;; by desired-type, mutable? and depth.
  (define (make-subexp-operator operator cache desired-type mutable? depth)
    (when disable-types? (set! desired-type (Any-type)))
    ;; Get the type information for the arguments
    (let ([domain-pairs
           (and (can-use-operator? operator orig-params depth desired-type)
                (operator-domain-with-mutability operator desired-type mutable?))]
          [cache-copy (and cache? (hash-copy cache))])
      
      (define (get-or-make-subexp pair)
        (let* ([simple-type (tm-pair-type pair)]
               [mutable? (tm-pair-mutable? pair)])
          (cached-grammar simple-type #:mutable? mutable? (- depth 1)
                                       cache-copy cache #t)))

      (define (special-andmap fn lst)
        (if (null? lst)
            lst
            (let* ([first (fn (car lst))]
                   [rest (and first (special-andmap fn (cdr lst)))])
              (and rest (cons first rest)))))

      ;; Generate subtrees for the arguments
      (let* ([result (and domain-pairs
                          (special-andmap get-or-make-subexp domain-pairs))])
        ;; Build the final tree
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

  ;; Dispatches to the appropriate make-subexp helper
  (define (make-subexp operator cache desired-type mutable? depth)
    (when disable-types? (set! desired-type (Any-type)))
    (cond [(grammar-operator? operator)
           (make-subexp-operator operator cache desired-type mutable? depth)]
          [(hash-has-key? special-form->proc operator)
           (define proc (hash-ref special-form->proc operator))
           (proc cache desired-type mutable? depth)]
          [else
           (error (format "Unknown special form: ~a" operator))]))

  ;; Generates all of the possible productions and chooses from all of
  ;; them. Returns a lifted? tree that satisfies the constraints
  ;; implied by desired-type, mutable? and depth.
  (define (grammar-general-helper desired-type depth #:mutable? [mutable? #f])
    (when disable-types? (set! desired-type (Any-type)))
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

  ;; Different ways in which the grammar might be used.
  (cond [(and (equal? mode 'stmt) (unify-types start-type (Void-type)))
         (build-grammar terminal-info num-stmts expr-depth num-temps guard-depth
                        grammar-general-helper
                        (lambda (num-stmts depth)
                          (build-list num-stmts
                                      (lambda (i)
                                        (grammar-general-helper (Void-type) depth)))))]
        [(equal? mode 'stmt)
         (grammar-general-helper start-type expr-depth #:mutable? mutable?)]
        [else
         (for/list ([i (second mode)])
           (create-temporary
            terminal-info start-type mutable?
            (lambda ()
              (grammar-general-helper start-type expr-depth #:mutable? mutable?))))]))

;; Basic grammar, which creates a lifted? object by generating a tree
;; using code similar in structure to a recursive descent parser. Each
;; new function requires another production and an edit to the code.
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

;; Adds bells and whistles to the simple "create a nested AST"
;; grammars that are defined above.
;; terminal-info: Lexical-Terminal-Info% object
;; num-stmts:     Number of statements the AST can have
;; depth:         Maximum allowed expression depth
;; num-temps:     The number of temporary variables to create. These
;;                are different from the temporary variables in the
;;                SSA grammar, because these are untyped.
;; guard-depth:   Depth of the guard expression. Zero means there
;;                should be no guard.
;; expr-grammar:  A function for generating symbolic expressions.
;; stmt-grammar:  A function for generating symbolic statements.
;; Generates a program of the following form:
;; (if <guard-expression>
;;     (void)
;;     (let ()
;;       (define temp-variable-1 (expr-??))
;;       ...
;;       (define temp-variable-%num-temps% (expr-??))
;;       (stmt-??)
;;       ...
;;       (stmt-??)))
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
        (send terminal-info make-and-add-terminal sym (infer-type subexp)
              #:mutable? #f))
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
(define Lexical-Terminal-Info%
  (class object%
    (super-new)

    (init-field [parent #f])
    (field [id->terminal (make-hash)])

    (define/public (has-id? id)
      (or (hash-has-key? id->terminal id)
          (and parent (send parent has-id? id))))

    (define/public (make-and-add-terminal sym type #:mutable? [mutable? #f])
      (define terminal (make-lifted-variable sym type #:mutable? mutable?))
      (add-terminal terminal)
      terminal)

    (define/public (add-terminal terminal)
      (define id (variable-symbol terminal))
      (when (has-id? id)
        (error (format "Terminal ~a is already present!~%" id)))
      (hash-set! id->terminal id terminal))

    (define/public (all-terminals)
      (if (not parent)
          (hash-values id->terminal)
          (append (hash-values id->terminal) (send parent all-terminals))))

    (define/public (get-terminal-by-id id)
      (if (hash-has-key? id->terminal id)
          (hash-ref id->terminal id)
          (send parent get-terminal-by-id id)))

    ;; Returns the terminals which are instances of subtypes of the argument
    ;; type, and which have the associated flags.
    (define/public (get-terminals #:type [type (Any-type)]
                                  #:mutable? [mutable? #f])
      (filter (lambda (terminal)
                (and (unify-types type (variable-type terminal))
                     (or (not mutable?)
                         (variable-mutable? terminal))))
              (all-terminals)))))
