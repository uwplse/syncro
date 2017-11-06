#lang racket

(require (only-in "../rosette/util.rkt" display-errors?))

(provide cmd-parse)

(define (ssa-spec? thing)
  (match thing
    [(list 'ssa (? number?)) #t]
    [_ #f]))

(define (cmd-parse [args (current-command-line-arguments)])
  (define valid-print-flags '(cegis debug progress stats))
  (define options
    (make-hash
     (list (cons 'logging (mutable-set 'cegis 'progress))
           ;(cons 'metasketch? #f)
           (cons 'no-type-analysis? #f)
           (cons 'no-mutability-analysis? #f)
           (cons 'cache? #t)
           (cons 'bitwidth 10)
           (cons 'timeout 3600)
           (cons 'module-file "metasketch-module-file.rkt")
           ;; grammar-version can be 'basic, 'general, 'caching, or
           ;; '(ssa n), where n is a non-negative integer (use '(ssa
           ;; 1) by default).
           (cons 'grammar-version '(ssa 1))
           ;; grammar-choice can be 'basic or 'sharing. 'sharing only
           ;; works with 'basic and 'general grammars.
           (cons 'grammar-choice 'basic))))
  (command-line
   #:argv args
   #:once-each
   [("-d" "--debug")
    "Execute with debugging information."
    (begin (display-errors? #t)
           (set-add! (hash-ref options 'logging) 'debug))]

   [("-s" "--stats")
    "Monitor and report various stats."
    (set-add! (hash-ref options 'logging) 'stats)]
   
   [("-v" "--verbose")
    "Execute with all printing except debug turned on."
    (hash-set! options 'logging (mutable-set 'cegis 'progress 'stats))]
   
   [("-q" "--quiet")
    "Execute with minimal messages."
    (hash-set! options 'logging (mutable-set))]
   
   #;[("-m" "--use-metasketch")
    "Use metasketches during synthesis."
      (hash-set! options 'metasketch? #t)]

   [("--no-types")
    "Do not use the type analysis."
    (hash-set! options 'no-type-analysis? #t)]

   [("--no-mutability")
    "Do not use the mutability analysis."
    (hash-set! options 'no-mutability-analysis? #t)]

   [("--no-cache")
    "Do not use caching."
    (hash-set! options 'cache? #f)]
   
   [("-b" "--bitwidth")
    bits
    ("The bitwidth to use in Rosette."
     "Must be an integer between 1 and 32.")
    (let ([bits-num (string->number bits)])
      (unless (and (integer? bits) (>= bits 1) (<= bits 32))
        (error (format "Invalid bitwidth: ~a" bits-num)))

      (hash-set! options 'bitwidth bits-num))]
   
   #;[("-t" "--timeout")
    secs
    ("The timeout for a single sketch to run in Synapse, in seconds."
     "Has no effect if -m is not also specified.")
    (hash-set! options 'timeout (string->number secs))]
   
   [("-g" "--grammar")
    grammar
    ("Which type of grammar to use."
     "Either basic, general, or (ssa n).")
    (let ([grammar-type (call-with-input-string grammar read)])
      (unless (or (member grammar-type '(basic general))
                  (ssa-spec? grammar-type))
        (error (format "Invalid option to -g or --grammar: ~a" grammar-type)))

      (hash-set! options 'grammar-version grammar-type))]
   
   [("-c" "--grammar-choice")
    choice
    ("Which type of choose* to use."
     "Either basic or sharing."
     "Sharing is incompatible with the ssa option for -g.")
    (let ([choice-sym (string->symbol choice)])
      (unless (member choice-sym '(basic sharing))
        (error (format "Invalid option to -c or --grammar-choice: ~a"
                       choice-sym)))

      (hash-set! options 'grammar-choice choice-sym))]

   #:multi
   [("-p" "--print")
    flag
    ("Turn on printing for the given flag."
     "Flags: cegis, debug, progress, stats"
     "cegis:    Print the guesses and counterexamples during synthesis."
     "debug:    Debugging messages."
     "progress: Print messages for each task that the algorithm performs."
     "stats:    Monitor and report various stats.")
    (let ([flag-sym (string->symbol flag)])
      (unless (member flag-sym valid-print-flags)
        (error (format "Invalid print flag: ~a" flag-sym)))

      (set-add! (hash-ref options 'logging) flag-sym)
      (when (equal? flag-sym 'debug)
        (display-errors? #t)))]

   ["--no-print"
    flag
    ("Turn off printing for the given flag."
     "See -p for available flags.")
    (let ([flag-sym (string->symbol flag)])
      (unless (member flag-sym valid-print-flags)
        (error (format "Invalid print flag: ~a" flag-sym)))

      (set-remove! (hash-ref options 'logging) flag-sym)
      (when (equal? flag-sym 'debug)
        (display-errors? #f)))]

   #:args () (void))

  (let ([version (hash-ref options 'grammar-version)]
        [choice (hash-ref options 'grammar-choice)])
    (when (and (or (equal? version 'caching) (ssa-spec? version))
               (equal? choice 'sharing))
      (error
       (format "Cannot combine grammar choice 'sharing with grammar type '~a"
               version))))
  options)
