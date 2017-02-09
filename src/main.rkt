#lang racket

(require "racket/read-file.rkt" "racket/synthesis.rkt")

(provide main)

(define (cmd-parse args)
  (define options
    (make-hash (list (cons 'debug? #f)
                     (cons 'verbose? #f)
                     (cons 'metasketch? #f)
                     (cons 'module-file "metasketch-module-file.rkt")
                     (cons 'grammar-version 'basic)
                     (cons 'grammar-choice 'basic))))
  
  (define prog
    (command-line
     #:argv args
     #:once-each
     [("-d" "--debug")
      "Execute with debugging information."
      (hash-set! options 'debug? #t)]
     
     [("-v" "--verbose")
      "Execute with verbose messages."
      (hash-set! options 'verbose? #t)]
     
     [("-m" "--use-metasketch")
      "Use metasketches during synthesis."
      (hash-set! options 'metasketch? #t)]
     
     [("-g" "--grammar")
      grammar-version
      "Which type of grammar to use"
      (hash-set! options 'grammar-version (string->symbol grammar-version))]
     
     [("-c" "--grammar-choice")
      grammar-choice
      "Which type of choose* to use"
      (hash-set! options 'grammar-choice (string->symbol grammar-choice))]
     
     #:args (filename)
     (read-file filename)))

  (values prog options))

(define (main . args)
  (define-values (prog options) (cmd-parse args))
  (define result (perform-synthesis prog options))
  (for-each pretty-print result))
