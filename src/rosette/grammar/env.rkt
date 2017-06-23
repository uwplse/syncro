#lang rosette

(require #;"../symhash.rkt" "../util.rkt")

(provide make-environment environment-ref environment-set)

;; We may in the future want an environment ADT that actually does
;; lexical scoping, but for now an environment just maps symbols to
;; values with no extra structure beyond that.

(define (make-environment [assocs '()])
  assocs)

(define (environment-ref env sym)
  (unless (member sym (map car env))
    (internal-error (format "Environment does not contain ~a!" sym)))
  (for/first ([assoc env] #:when (equal? (car assoc) sym))
    (cdr assoc)))

(define (environment-set env sym value)
  (if (member sym (map car env))
      (for/list ([assoc env])
        (if (equal? (car assoc) sym)
            (cons sym value)
            assoc))
      (cons (cons sym value) env)))

;; (define (make-environment [assocs '()])
;;   (rhash assocs))

;; (define (environment-ref env sym)
;;   (unless (rhash-has-key? env sym)
;;     (internal-error (format "Environment does not contain ~a!" sym)))
;;   (rhash-ref env sym))

;; (define (environment-set env sym val)
;;   (rhash-set env sym val))

;; (define (merge-env env)
;;   (if (not (union? env))
;;       env
;;       (let* ([gv-pairs (union-contents env)]
;;              [guards (map car gv-pairs)]
;;              [envs (map cdr gv-pairs)])
;;         (unless (andmap rhash? envs)
;;           (internal-error
;;            (format "Expected a symbolic union of environments, got ~a"
;;                    env)))

;;         (define list-of-keylists (map rhash-keys envs))
;;         (unless (= (set-count (apply set list-of-keylists)) 1)
;;           (internal-error
;;            (format "Expected all environments to have the same keys, but got ~a"
;;                    list-of-keylists)))

;;         ;; TODO: Construct a merged hash here
;;         )))
                          
