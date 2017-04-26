#lang racket

(require (only-in "../rosette/util.rkt" display-errors?))

(provide cmd-parse)

(define (cmd-parse [args (current-command-line-arguments)])
  (define options
    (make-hash (list (cons 'debug? #f)
                     (cons 'verbose? #t)
                     (cons 'metasketch? #f)
                     (cons 'bitwidth 10)
                     (cons 'timeout 3600)
                     (cons 'module-file "metasketch-module-file.rkt")
                     ;; grammar-version can be 'basic, 'general,
                     ;; 'caching, or '(ssa n), where n is a
                     ;; non-negative integer (use '(ssa 1) by
                     ;; default).
                     (cons 'grammar-version '(ssa 1))
                     ;; grammar-choice can be 'basic or 'sharing.
                     ;; 'sharing only works with 'basic and 'general
                     ;; grammars.
                     (cons 'grammar-choice 'basic))))
  (command-line
   #:argv args
   #:once-each
   [("-d" "--debug")
    "Execute with debugging information."
    (begin (display-errors? #t)
           (hash-set! options 'debug? #t))]
   
   [("-v" "--verbose")
    "Execute with verbose messages."
    (hash-set! options 'verbose? #t)]
   
   [("-q" "--quiet")
    "Execute with minimal messages."
    (hash-set! options 'verbose? #f)]
   
   [("-m" "--use-metasketch")
    "Use metasketches during synthesis."
    (hash-set! options 'metasketch? #t)]
   
   [("-b" "--bitwidth")
    bits
    "The bitwidth to use in Rosette. Must be an integer between 1 and 32."
    (hash-set! options 'bitwidth (string->number bits))]
   
   [("-t" "--timeout")
    secs
    ("The timeout for a single sketch to run in Synapse, in seconds."
     "Has no effect if -m is not also specified.")
    (hash-set! options 'timeout (string->number secs))]
   
   [("-g" "--grammar")
    grammar
    "Which type of grammar to use -- either basic, general or caching."
    (hash-set! options 'grammar-version (string->symbol grammar))]
   
   [("-c" "--grammar-choice")
    choice
    "Which type of choose* to use"
    (hash-set! options 'grammar-choice (string->symbol choice))]
   
   #:args () (void))

  options)
