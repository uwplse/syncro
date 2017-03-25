#lang racket
(require (for-syntax racket/base syntax/parse)
         "../src/racket/constructs.rkt")
(provide
 (except-out (all-from-out racket) #%module-begin)
 (all-from-out "../src/racket/constructs.rkt")
 (rename-out [new-mb #%module-begin]))

(define-syntax new-mb
  (syntax-parser [(_ . es) #'(#%module-begin (incremental . es))]))
