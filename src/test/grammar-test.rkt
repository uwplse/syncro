#lang rosette

(require rackunit rackunit/text-ui)
(require "../rosette/grammar/grammar.rkt"
         "../rosette/grammar/language.rkt"
         "../rosette/types.rkt" "../rosette/variable.rkt"
         (only-in "../rosette/grammar/lifted-operators.rkt"
                  special-form? operator-info))

(provide run-grammar-tests)

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
       (let ([new-ops (remove-polymorphism operator-info info)])
         (check-equal? (for/set ([op new-ops] #:unless (special-form? op))
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
       (program (list '(vector-decrement! num1 wild1))))

     (define simple-prog2
       (prog-substitute template2 'wild1 '(vector-ref word->topic word1)))

     ;; num1 cannot be indexed by a word
     (define impossible-prog3
       (prog-substitute template2 'wild1 'word1))

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
     
     (test-case "Grammars"
       (for ([config configs])
         (time
          (match-define (list grammar-version choice-version test-medium?) config)
          (printf "Testing the ~a grammar with choice version ~a~%"
                  grammar-version choice-version)
          (define simple-grmr (grammar info operator-info 2 2
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
            (define medium-grmr (grammar info operator-info 3 4
                                         #:version grammar-version
                                         #:choice-version choice-version))
            (check-in-grammar medium-grmr (prog->sexp medium-prog5 3))))))
   )))

(define (run-grammar-tests)
  (displayln "Running tests for grammar.rkt")
  (run-tests tests))
