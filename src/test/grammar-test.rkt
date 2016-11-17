#lang rosette

(require rackunit rackunit/text-ui)
(require "../grammar.rkt" "../rosette-util.rkt" "../types.rkt" "../variable.rkt")

(provide run-grammar-tests)

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
       (check-equal? (get #:type int) (set 'x 'y 'z))
       (check-equal? (get #:type int #:mutable? #t) (set 'x))
       (check-equal? (get #:type idx #:mutable? #t) (set 'v 'x))
       (check-equal? (get #:type bool) (set))
       (check-equal? (get #:type any) (set 'v 'w 'x 'y 'z))))

   (test-case "Grammar construction"
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

       (define simple-prog (grammar info 2 2))
       (define medium-prog (grammar info 3 4))

       (check-in-grammar
        simple-prog
        '(let () (void)))

       (check-in-grammar
        simple-prog
        '(let ()
           (let ()
             (vector-decrement! num1 (vector-ref word->topic word1))
             (void))))

       ;; Beware, it is VERY EASY to create code that "accidentally"
       ;; can't be synthesized. Ideally there should be a program that
       ;; can be synthesized, and you modify it *very* slightly.
       (check-not-in-grammar
        simple-prog
        '(let ()
           (let ()
             ;; num1 cannot be indexed by a word
             (vector-decrement! num1 word1)
             (void))))

       (check-in-grammar
        simple-prog
        '(let ()
           (let ()
             (vector-set! num1 (vector-ref word->topic word1) 0)
             (let ()
               (vector-decrement! num1
                                  (vector-ref word->topic word2))
               (void)))))

       (define medium-example
         '(let ()
            (let ()
              (vector-set!
               num1
               (vector-ref word->topic word1)
               (* 2 (vector-ref num1 (vector-ref word->topic word1))))
              (let ()
                (vector-decrement! num1
                                   (vector-ref word->topic word2))
                (void)))))
       
       (check-not-in-grammar simple-prog medium-example)
       (check-in-grammar medium-prog medium-example)))
   ))

(define (run-grammar-tests)
  (displayln "Running tests for grammar.rkt")
  (run-tests tests))
