#lang rosette

(require rackunit rackunit/text-ui)
(require "../rosette/grammar/grammar.rkt"
         "../rosette/grammar/language.rkt"
         "../rosette/types.rkt"
         "../rosette/util.rkt"
         "../rosette/variable.rkt"
         (only-in "../rosette/grammar/grammar-operators.rkt"
                  default-operators))

(provide run-grammar-tests)

(define (wild-equal? x y)
  (or (and (equal? x 'wild) (not (pair? y)))
      (and (equal? y 'wild) (not (pair? x)))
      (eq? x y)
      (and (pair? x) (pair? y)
           (= (length x) (length y))
           (not (member #f (map wild-equal? x y))))))

(define (substitute lst old new)
  (cond [(equal? lst old) new]
        [(not (pair? lst)) lst]
        [else (cons (substitute (car lst) old new)
                    (substitute (cdr lst) old new))]))

(struct program (stmts))
(define (prog-substitute prog old new)
  (program (substitute (program-stmts prog) old new)))

(define tests
  (test-suite
   "Tests for grammar.rkt"

   (test-case "Terminal info"
     (let* ([info (new Lexical-Terminal-Info%)]
            [int (Integer-type)]
            [idx (Index-type)]
            [bool (Boolean-type)]
            [any (Any-type)]
            [vec (Vector-type int bool)]
            [z (make-lifted-variable 'z int)])
       
       (define (make a b #:mutable? m)
         (send info make-and-add-terminal a b #:mutable? m))
       
       (define (get #:type type #:mutable? [m #f])
         (list->set
          (map variable-symbol
               (send info get-terminals #:type type #:mutable? m))))

       (make 'v idx #:mutable? #t)
       (make 'w any #:mutable? #f)
       (make 'x int #:mutable? #t)
       (make 'y int #:mutable? #f)
       (send info add-terminal z)
       
       (check-equal? (send info get-terminal-by-id 'z) z)
       (check-equal? (get #:type int) (set 'v 'w 'x 'y 'z))
       (check-equal? (get #:type int #:mutable? #t) (set 'v 'x))
       (check-equal? (get #:type idx #:mutable? #t) (set 'v 'x))
       (check-equal? (get #:type bool) (set 'w))
       (check-equal? (get #:type any) (set 'v 'w 'x 'y 'z))))

   ;; Define a Terminal-info for the next set of tests
   (let* ([info (new Lexical-Terminal-Info%)]
          [int (Integer-type)]
          [Word (Enum-type 'Word 12)]
          [Topic (Enum-type 'Topic 3)]
          [word->topic-type (Vector-type Word Topic)]
          [num1-type (Vector-type Topic int)]
          [related-word-type (Vector-type Word (Vector-type Topic Word))])
       
     (define (make a b #:mutable? m)
       (send info make-and-add-terminal a b #:mutable? m))

     (make 'word->topic word->topic-type #:mutable? #f)
     (make 'num1 num1-type #:mutable? #t)
     (make 'related-word related-word-type #:mutable? #f)

     (make 'int1 int #:mutable? #f)
     (make 'word1 Word #:mutable? #f)
     (make 'word2 Word #:mutable? #t)

     ;; TODO: More removing polymorphism tests
     (test-case "Removing polymorphism"
       (let-values ([(a b new-ops) (remove-polymorphism default-operators info)])
         (check-equal? (for/set ([op new-ops] #:when (variable? op))
                         (variable-symbol op))
                       (set 'equal? 'void 'vector-ref 'vector-set!
                            'vector-increment! 'vector-decrement!
                            '+ '- '* '< '=
                            'and 'or 'not))))

     ;; If in-grammar? is #f, checks that prog *cannot* be specialized to code
     ;; If it is #t, checks that prog *can* be specialized to code
     (define (check-grammar in-grammar? prog code)
       ;; Synthesize a program from prog that matches code
       (define solution
         (solve (assert (equal? (lifted-code prog) code))))
         
       ;; Check that synthesis succeeded
       (check-equal? (sat? solution) in-grammar?
                     (format "Grammar should ~ainclude: ~a"
                             (if in-grammar? "" "not ")
                             code)))

     (define check-in-grammar (curry check-grammar #t))
     (define check-not-in-grammar (curry check-grammar #f))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Programs in the grammar ;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     ;; Represent a program as a list of statements
     (define simple-prog1
       (program (list)))
     
     (define template2
       (program (list '(vector-decrement! num1 dummy))))

     (define simple-prog2
       (prog-substitute template2 'dummy '(vector-ref word->topic word1)))

     ;; num1 cannot be indexed by a word
     (define impossible-prog3
       (prog-substitute template2 'dummy 'word1))

     (define simple-prog4
       (program
        (list '(vector-set! num1 (vector-ref word->topic word1) 0)
              '(vector-decrement! num1 (vector-ref word->topic word2)))))

     (define medium-prog5
       (program
        (list '(vector-set! num1 (vector-ref word->topic word1)
                            (* 2 (vector-ref num1 (vector-ref word->topic word1))))
              '(vector-decrement! num1 (vector-ref word->topic word2)))))

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; Conform programs to grammar ;;
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     (define (make-basic-sexp prog num-stmts)
       ;; A program from the basic grammar has the form
       ;; (let () (let () stmt1 (let () stmt2 (void))))
       (define (loop stmts)
         (if (null? stmts)
             '(void)
             `(let ()
                ,(car stmts)
                ,(loop (cdr stmts)))))
       
       `(let () ,(loop (program-stmts prog))))

     (define (make-general-sexp prog num-stmts)
       ;; A program from the general grammar has the form
       ;; (let () stmt1 stmt2 ... stmtN)
       (define voids (build-list num-stmts (lambda (i) '(void))))
       (define stmts (take (append (program-stmts prog) voids) num-stmts))
       `(let () ,@stmts))

     ;;;;;;;;;;;;;;;;;;;;
     ;; Configurations ;;
     ;;;;;;;;;;;;;;;;;;;;
     
     (define configs
       (list '(basic basic #f #t)
             '(basic sharing #f #t)
             '(general basic #f #t)
             '(general sharing #f #t)
             '(general basic #t #t)
             #;'(synthax-deep basic #f)))

     ;; Grammar types to be combined with the mock chooser
     (define concrete-configs
       (list 'basic 'general '(ssa 2)))

     ;;;;;;;;;;;;;;;;;;;
     ;; Running tests ;;
     ;;;;;;;;;;;;;;;;;;;

     (test-case "No errors when running grammars"
       ;; Make sure that calling a grammar with a mock chooser
       ;; succeeds and returns some kind of lifted program
       (for ([grammar-version concrete-configs])
         (check-true
          (lifted? (grammar info 2 2
                            #:version grammar-version
                            #:choice-version 'mock)))))
     
     (test-case "Basic, General, and Caching Grammars"
       (for ([config configs])
         (time
          (match-define (list grammar-version choice-version cache? test-medium?) config)
          (printf "Testing the ~a grammar with choice version ~a ~a caching~%"
                  grammar-version choice-version (if cache? "with" "without"))
          (clear-state!)
          (define simple-grmr (grammar info 2 2
                                       #:version grammar-version
                                       #:choice-version choice-version))

          (define (prog->sexp prog num-stmts)
            (if (member grammar-version '(basic synthax-deep))
                (make-basic-sexp prog num-stmts)
                (make-general-sexp prog num-stmts)))
          
          (check-in-grammar simple-grmr (prog->sexp simple-prog1 2))
          (check-in-grammar simple-grmr (prog->sexp simple-prog2 2))
          (check-not-in-grammar simple-grmr (prog->sexp impossible-prog3 2))
          (check-in-grammar simple-grmr (prog->sexp simple-prog4 2))
          (check-not-in-grammar simple-grmr (prog->sexp medium-prog5 2))

          (when test-medium?
            (clear-state!)
            (define medium-grmr (grammar info 3 4
                                         #:version grammar-version
                                         #:choice-version choice-version))
            (check-in-grammar medium-grmr (prog->sexp medium-prog5 3))))))

     (test-case "SSA grammar"
       (printf "Testing the SSA grammar~%")
       (clear-state!)

       (define (get-defines-and-stmts prog)
         (partition (lambda (x) (and (list? x) (equal? 'define (car x))))
                    (program-stmts prog)))

       (define (assert-all-in lst1 lst2 [eq-fn equal?])
         (for ([elem1 lst1])
           (assert (my-for/or ([elem2 lst2])
                     (eq-fn elem1 elem2)))))

       (define (check-ssa-grammar in-grammar? prog expected-prog num-stmts)
         (define-values (expected-defs stmts)
           (get-defines-and-stmts expected-prog))
         (define expected-stmts
           (make-general-sexp (program stmts) num-stmts))

         (define prog-code (lifted-code prog))
         (define prog-stmts (last prog-code))

         (define solution
           (solve
            (begin (assert-all-in expected-defs prog-code wild-equal?)
                   (assert (wild-equal? expected-stmts prog-stmts)))))

         (define code `(let () ,@expected-defs ,expected-stmts))
         (check-equal? (sat? solution) in-grammar?
                       (format "Grammar should~ainclude: ~a"
                               (if in-grammar? " " " not ")
                               code)))

       (define check-in-ssa (curry check-ssa-grammar #t))
       (define check-not-in-ssa (curry check-ssa-grammar #f))

       ;; simple-prog4 will not be synthesized because it involves a
       ;; numeric constant (0).
       (define simple-grmr
         (grammar info 2 2 #:version '(ssa 2)))

       (check-in-ssa simple-grmr simple-prog1 2)
       (check-in-ssa simple-grmr simple-prog2 2)
       (check-not-in-ssa simple-grmr impossible-prog3 2)
       ;; For all of the programs that use temporary variables, make
       ;; sure that we do not depend on the order in which we generate
       ;; temporary variables for types. For every test, make sure we
       ;; write down exactly which iteration each temporary variable
       ;; would come from.
       (define ssa-prog4
         (program
          (list '(define wild 0)
                '(define wild (vector-ref word->topic word1)) ;; Iter 1
                '(define wild (vector-ref word->topic word2)) ;; Iter 2
                '(vector-set! num1 wild wild)
                '(vector-decrement! num1 wild))))
       (check-in-ssa simple-grmr ssa-prog4 2)

       (clear-state!)

       (define medium-prog5
         (program
          (list '(define wild 2)
                '(define wild (vector-ref word->topic word1)) ;; Iter 1
                ;; First or second iteration, depending on order
                '(define wild (vector-ref num1 wild)) ;; Iter 1 or 2
                '(define wild (vector-ref word->topic word2)) ;; Iter 2
                '(define wild (* wild wild)) ;; Iter 3
                '(vector-set! num1 wild wild)
                '(vector-decrement! num1 wild))))

       (define medium-grmr (grammar info 2 3 #:version '(ssa 2)))
       (check-in-ssa medium-grmr medium-prog5 2)))))

(define (run-grammar-tests)
  (displayln "Running tests for grammar.rkt")
  (run-tests tests))

(module+ main
  (run-grammar-tests))
