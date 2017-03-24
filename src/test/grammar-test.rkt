#lang rosette

(require rackunit rackunit/text-ui)
(require "../rosette/grammar/grammar.rkt"
         "../rosette/grammar/language.rkt"
         "../rosette/types.rkt" "../rosette/variable.rkt"
         (only-in "../rosette/grammar/grammar-operators.rkt"
                  default-operators))

(provide run-grammar-tests)

(define (wild-equal? x y)
  (or (and (equal? x 'wild) (symbol? y))
      (and (equal? y 'wild) (symbol? x))
      (eq? x y)
      (and (= (length x) (length y))
           (not (member #f (map wild-equal? x y))))))

(define (wild-void-equal? x y)
  (wild-equal? (filter (negate (curry equal? '(void))) x)
               (filter (negate (curry equal? '(void))) y)))

;; lifted-thing must be a lifted-begin
;; Dead code is replaced by (void) so that the overall code has the
;; same length (the length isn't symbolic), which makes symbolic
;; evaluation much more efficient.
(define (get-code-without-dead-code lifted-thing)
  (unless (lifted-begin? lifted-thing)
    (error "Can only remove dead code from a lifted begin"))
  (cons
   'let
   (cons
    '()
    (let loop ([args (reverse (lifted-begin-args lifted-thing))]
               [symbols '()] [result '()])

      (define (ignore? item)
        (and (lifted-define? item)
             (not (member (variable-symbol (lifted-define-var item))
                          symbols))))

      (define (get-symbols item)
        (fold-lifted item
                     (lambda (x)
                       (if (lifted-variable? x)
                           (list (variable-symbol x))
                           '()))
                     append))

      (cond [(null? args) result]
            [(ignore? (car args))
             (loop (cdr args) symbols (cons '(void) result))]
            [else
             (loop (cdr args)
                   (append (get-symbols (car args)) symbols)
                   (cons (lifted-code (car args)) result))])))))

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
     (let* ([info (new Terminal-Info%)]
            [int (Integer-type)]
            [idx (Index-type)]
            [bool (Boolean-type)]
            [any (Any-type)]
            [vec (Vector-type int bool)]
            [z (make-lifted-variable 'z int #:value 3)])
       
       (define (make a b c #:mutable? m)
         (send info make-and-add-terminal a b c #:mutable? m))
       
       (define (get #:type type #:mutable? [m #f])
         (list->set
          (map variable-symbol
               (send info get-terminals #:type type #:mutable? m))))

       (make 'v 1 idx #:mutable? #t)
       (make 'w "foo" any #:mutable? #f)
       (make 'x 3 int #:mutable? #t)
       (make 'y 4 int #:mutable? #f)
       (send info add-terminal z)
       
       (check-equal? (send info get-terminal-by-id 'z) z)
       (check-equal? (get #:type int) (set 'v 'w 'x 'y 'z))
       (check-equal? (get #:type int #:mutable? #t) (set 'v 'x))
       (check-equal? (get #:type idx #:mutable? #t) (set 'v 'x))
       (check-equal? (get #:type bool) (set 'w))
       (check-equal? (get #:type any) (set 'v 'w 'x 'y 'z))))

   ;; Define a Terminal-info for the next set of tests
   (let* ([info (new Terminal-Info%)]
          [int (Integer-type)]
          [Word (Enum-type 'Word 12)]
          [Topic (Enum-type 'Topic 3)]
          [word->topic-type (Vector-type Word Topic)]
          [num1-type (Vector-type Topic int)]
          [related-word-type (Vector-type Word (Vector-type Topic Word))])
       
     (define (make a b c #:mutable? m)
       (send info make-and-add-terminal a b c #:mutable? m))

     (define word->topic (build-vector 12 (lambda (i) (remainder i 3))))
     (make 'word->topic word->topic word->topic-type #:mutable? #f)
     (define num1 (make-vector 3 4))
     (make 'num1 num1 num1-type #:mutable? #t)
     (define related-word (build-vector 12 (lambda (i) (make-vector 3 0))))
     (make 'related-word related-word related-word-type #:mutable? #f)

     (make 'int1 0 int #:mutable? #f)
     (make 'word1 3 Word #:mutable? #f)
     (make 'word2 5 Word #:mutable? #t)

     ;; TODO: More removing polymorphism tests
     (test-case "Removing polymorphism"
       (let-values ([(a b new-ops) (remove-polymorphism default-operators info)])
         (check-equal? (for/set ([op new-ops] #:when (variable? op))
                         (variable-symbol op))
                       (set 'equal? 'void 'vector-ref 'vector-set!
                            'vector-increment! 'vector-decrement!
                            '+ '- '* '< '=))))

     ;; If in-grammar? is #f, checks that prog *cannot* be specialized to code
     ;; If it is #t, checks that prog *can* be specialized to code
     (define (check-grammar in-grammar? prog code)
       ;; Synthesize a program from prog that matches code
       (define solution
         (solve (assert (equal? (lifted-code prog) code))))
         
       ;; Check that synthesis succeeded
       (check-equal? (sat? solution) in-grammar?
                     (format (if in-grammar?
                                 "Grammar should include: ~a"
                                 "Grammar should not include: ~a")
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
       (list '(basic basic #t)
             '(basic sharing #t)
             '(general basic #t)
             '(general sharing #t)
             '(caching basic #t)
             #;'(synthax-deep basic #f)))

     ;;;;;;;;;;;;;;;;;;;
     ;; Running tests ;;
     ;;;;;;;;;;;;;;;;;;;
     
     (test-case "Basic, General, and Caching Grammars"
       (for ([config configs])
         (time
          (match-define (list grammar-version choice-version test-medium?) config)
          (printf "Testing the ~a grammar with choice version ~a~%"
                  grammar-version choice-version)
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

     #;(test-case "SSA grammar"
       (printf "Testing the SSA grammar~%")
       (clear-state!)

       ;; Creates code for the SSA grammar assuming there are no
       ;; temporary variables.
       (define (make-ssa-sexp prog num)
         (define-values (defines stmts)
           (partition (lambda (x) (and (list? x) (equal? 'define (car x))))
                      (program-stmts prog)))
         `(let ()
            ,@defines
            ,(make-general-sexp (program stmts) num)))

       (define (check-ssa-grammar in-grammar? prog-code code)
         (define solution
           (solve (assert (wild-void-equal? prog-code code))))
         (check-equal? (sat? solution) in-grammar?
                       (format (if in-grammar?
                                   "Grammar should include: ~a"
                                   "Grammar should not include: ~a")
                               code)))
       (define check-in-ssa (curry check-ssa-grammar #t))
       (define check-not-in-ssa (curry check-ssa-grammar #f))

       ;; simple-prog4 will not be synthesized because it involves a
       ;; numeric constant (0).
       (define simple-grmr (grammar info 2 2 #:version '(ssa 2)))
       (define simple-code (time (get-code-without-dead-code simple-grmr)))
       (check-in-ssa simple-code (make-ssa-sexp simple-prog1 2))
       (check-in-ssa simple-code (make-ssa-sexp simple-prog2 2))
       (check-not-in-ssa simple-code (make-ssa-sexp impossible-prog3 2))

       (clear-state!)
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
       (check-in-ssa simple-code (make-ssa-sexp simple-prog1 2))

       (define prog5-list
         (list '(define wild 2)
               '(define wild (vector-ref word->topic word1)) ;; Iter 1
               ;; First or second iteration, depending on order
               '(define wild (vector-ref num1 wild)) ;; Iter 1 or 2
               '(define wild (vector-ref word->topic word2)) ;; Iter 2
               '(define wild (* wild wild)) ;; Iter 3
               '(vector-set! num1 wild wild)
               '(vector-decrement! num1 wild)))
       ;; The program above has 7 statements -- 1 constant, 4
       ;; temporary variables, and 2 statements. Here are the
       ;; dependencies:
       ;; Nothing necessary for 1 or 2
       ;; 2 is necessary for 3
       ;; Nothing necessary for 4
       ;; Either 1 or 3 is necessary for 5
       ;; (2 or 4) and (3 or 5) is necessary for 6
       ;; (2 or 4) is necessary for 7

       (define medium-prog5 (program prog5-list))
       ;; Drop statement 2, should become impossible
       (define impossible-prog6
         (program (cons (first prog5-list) (drop prog5-list 2))))
       ;; Drop 3 and 5, should become impossible
       (define impossible-prog7
         (program (append (take prog5-list 2) ; 1 and 2
                          (list (fourth prog5-list)) ; 4
                          (drop prog5-list 5)))) ; 6 and 7
       ;; Drop statement 7, should become impossible
       ;; (no way to use both 2 and 4, so one would be eliminated)
       (define impossible-prog8
         (program (take prog5-list 6)))
       ;; Dropping statement 1 is okay, because then 5 will just
       ;; multiply two copies of 3.
       (define medium-prog9
         (program (cdr prog5-list)))

       (define medium-grmr (grammar info 2 3 #:version '(ssa 2)))
       (define medium-code (time (get-code-without-dead-code medium-grmr)))
       (displayln "Check test of test")
       (check-in-ssa medium-code
                     '(let () wild (let () wild wild)))
       (displayln "Check prog5")
       (check-in-ssa medium-code (make-ssa-sexp medium-prog5 2))
       (displayln "Check prog6")
       (check-not-in-ssa medium-code (make-ssa-sexp impossible-prog6 2))
       (check-not-in-ssa medium-code (make-ssa-sexp impossible-prog7 2))
       (check-not-in-ssa medium-code (make-ssa-sexp impossible-prog8 2))
       (check-in-ssa medium-code (make-ssa-sexp medium-prog9 2))))))

(define (run-grammar-tests)
  (displayln "Running tests for grammar.rkt")
  (run-tests tests))
