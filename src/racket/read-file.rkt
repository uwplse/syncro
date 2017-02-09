#lang racket

(require "dependency-graph.rkt"
         "../rosette/rosette-namespace.rkt"
         "../rosette/variable.rkt")

(provide read-file
         program program? program-initialization program-constants
         program-dependency-graph program-algorithm)

(struct program (initialization constants dependency-graph algorithm) #:transparent #:mutable)
(define (add-initialization prog code)
  (program code (program-dependency-graph prog) (program-algorithm prog)))
(define (add-graph prog graph)
  (program (program-initialization prog) graph (program-algorithm prog)))

(define (add-node-to-prog! prog name type-expr updates expr)
  (add-node! (program-dependency-graph prog) name
             (new node%
                  [id name]
                  [type (run-in-rosette type-expr)]
                  [update-types updates]
                  [fn-code expr])))

(define (add-dependencies! prog parents name)
  (for ([parent parents])
    (add-dependency! (program-dependency-graph prog) parent name)))

(define (handle-initialization code prog)
  (define new-code
    (match code
      [`(define ,var ,type-exp ,val)
       (define type (run-in-rosette type-exp))
       (set-program-constants!
        prog
        (cons (make-variable var #:type type #:definition `(define ,var ,val))
              (program-constants prog)))
       `(define ,var ,val)]
      [`(define ,var ,val)
       (error "Normal define not allowed, either provide a type or use define-no-synthesis")]
      [`(define-no-synthesis ,var ,val)
       (set-program-constants!
        prog
        (cons (make-variable var #:definition code) (program-constants prog)))
       code]
      [`(define-enum-type ,var ,items)
       (define desugared-code `(define ,var (Enum-type ',var ,items)))
       (set-program-constants!
        prog
        (cons (make-variable var #:definition desugared-code)
              (program-constants prog)))
       desugared-code]
      [_ code]))

  (run-in-rosette new-code)
  (set-program-initialization!
   prog
   (cons new-code (program-initialization prog))))

(define (handle-data-structure code prog)
  (match code
    [`(define-incremental ,name ,type-exp (,parent ...) (,update-type ...) ,expr ...)
     (add-node-to-prog! prog name type-exp update-type `(begin ,@expr))
     (add-dependencies! prog parent name)]))

(define (handle-algorithm code prog)
  (set-program-algorithm! prog (cons code (program-algorithm prog))))

;; Returns a program struct.
(define (read-file filename)
  (define prog (program '() '() (make-dependency-graph) '()))
  
  ;; Hacky solution. Each procedure knows which one to call next, and
  ;; that knowledge is interleaved with the program flow.
  (define (read-initialization code)
    (cond [(eof-object? code)
           (error "End of file before data structures")]
          [(member (car code) '(define-incremental define-mutable))
           (read-data-structures code)]
          [else
           (define result (handle-initialization code prog))
           (read-initialization (read))]))
  
  (define (read-data-structures code)
    (cond [(eof-object? code)
           (error "End of file before algorithm")]
          [(member (car code) '(define-incremental define-mutable))
           (handle-data-structure code prog)
           (read-data-structures (read))]
          [else
           (read-algorithm code)]))
  
  (define (read-algorithm code)
    (cond [(eof-object? code) 'done]
          [else
           (handle-algorithm code prog)
           (read-algorithm (read))]))

  (with-input-from-file filename
    (thunk (read-initialization (read))))

  (program (reverse (program-initialization prog))
           (reverse (program-constants prog))
           (program-dependency-graph prog)
           (reverse (program-algorithm prog))))
     
