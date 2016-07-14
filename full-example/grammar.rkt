#lang s-exp rosette

(require rosette/lib/angelic)

(require "types.rkt" "rosette-util.rkt")

(provide vector-sum vector-increment! vector-decrement!
         stmt-grammar Terminal-Info%
         eval-lifted lifted-code)

(define (vector-sum vec)
  (define result 0)
  (for ([v vec])
    (set! result (+ result v)))
  result)

(define (vector-increment! vec index)
  (vector-set! vec index
               (+ (vector-ref vec index) 1)))

(define (vector-decrement! vec index)
  (vector-set! vec index
               (- (vector-ref vec index) 1)))

(define-lifted [void void^]
  [vector-increment! vector-increment!^] [vector-decrement! vector-decrement!^]
  [vector-set! vector-set!^] [vector-ref vector-ref^])
  
(define (stmt-grammar terminal-info num-stmts stmt-depth)
  (if (= num-stmts 0)
      (void^)
      (choose* (void^)
               (begin^ (base-stmt-grammar terminal-info stmt-depth)
                       (stmt-grammar terminal-info (- num-stmts 1) stmt-depth)))))

(define (base-stmt-grammar terminal-info depth)
  (vector-stmt-grammar terminal-info depth))

(define (vector-stmt-grammar terminal-info depth)
  (let* ([vec (apply choose*
                     (send terminal-info get-lifted-terminals
                           #:type Vector%
                           'writable))]
         [vec-type (send terminal-info get-type vec)]
         [index (index-expr-grammar terminal-info (- depth 1))]
         [value (apply choose*
                       (send terminal-info get-lifted-terminals
                             #:type (send vec-type get-output-type)))])
    (choose* ((choose* vector-increment!^ vector-decrement!^) vec index)
             (vector-set!^ vec index value))))

;; TODO: Create a single expr-grammar that takes a type as input and
;; produces expressions that would create a value of that type.
(define (index-expr-grammar terminal-info depth)
  (if (= depth 0)
      (apply choose* (send terminal-info get-lifted-terminals
                           #:type Integer%))
      (choose* (apply choose* (send terminal-info get-lifted-terminals
                                    #:type Integer%))
               (vector-ref^ (apply choose* (send terminal-info get-lifted-terminals
                                                 #:type Vector%))
                            (index-expr-grammar terminal-info (- depth 1))))))


(define Terminal-Info%
  (class object%
    (super-new)

    (field [terminal->value (make-hash)]
           [terminal->type (make-hash)]
           [terminal->flags (make-hash)]
           [all-flags (set 'writable 'read-only)])

    (define/public (add-terminal symbol value type #:writable [writable #f])
      (when (hash-has-key? terminal->type symbol)
        (error (format "Terminal ~a is already present!~%" symbol)))

      (hash-set! terminal->value symbol value)
      (hash-set! terminal->type symbol type)
      (hash-set! terminal->flags symbol
                 (set (if writable 'writable 'read-only))))

    (define/public (get-value terminal)
      (when (lifted-value? terminal)
        (set! terminal (lifted-code terminal)))
      (unless (hash-has-key? terminal->value terminal)
        (error (format "Unknown terminal ~a in get-value~%" terminal)))
      (hash-ref terminal->value terminal))

    (define/public (get-type terminal)
      (when (lifted-value? terminal)
        (set! terminal (lifted-code terminal)))
      (unless (hash-has-key? terminal->type terminal)
        (error (format "Unknown terminal ~a in get-type~%" terminal)))
      (hash-ref terminal->type terminal))

    ;; Returns the terminals which is an instance of subtype of the argument
    ;; type, and which has the associated flags.
    ;; Type can be either a type class or a type instance.
    (define/public (get-terminals #:type [type Type%] . flags)
      (define flags-set (list->set flags))
      (unless (subset? flags-set all-flags)
        (error (format "Unrecognized flag(s): ~a~%"
                       (set->list (set-subtract flags-set
                                                all-flags)))))
      
      (filter (lambda (sym)
                (let ([sym-type (hash-ref terminal->type sym)]
                      [sym-flags (hash-ref terminal->flags sym)])
                  (and (send sym-type is-supertype? type)
                       (subset? flags-set sym-flags))))
              (hash-keys terminal->type)))

    (define/public (get-lifted-terminals #:type [type Type%] . flags)
      (define terminals (send/apply this get-terminals #:type type flags))
      (map lift
           (map (lambda (t) (get-value t)) terminals)
           terminals))))
