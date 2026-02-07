#lang racket

(provide make-env
         env-lookup
         env-set!
         env-update!
         env-extend)

;; An environment is a mutable hash table with an optional parent.
(struct env (bindings parent) #:transparent)

(define (make-env [parent #f])
  (env (make-hash) parent))

(define (env-set! e sym val)
  (hash-set! (env-bindings e) sym val))

(define (env-update! e sym val)
  (cond
    [(hash-has-key? (env-bindings e) sym)
     (hash-set! (env-bindings e) sym val)]
    [(env-parent e)
     (env-update! (env-parent e) sym val)]
    [else
     (error (format "cannot set! unbound variable: ~a" sym))]))

(define (env-extend parent params args)
  (unless (= (length params) (length args))
    (error (format "arity mismatch: expected ~a, got ~a"
                   (length params) (length args))))
  (define child (make-env parent))
  (for ([p params] [a args])
    (env-set! child p a))
  child)

(define (env-lookup e sym)
  (cond
    [(hash-has-key? (env-bindings e) sym)
     (hash-ref (env-bindings e) sym)]
    [(env-parent e)
     (env-lookup (env-parent e) sym)]
    [else
     (error (format "unbound variable: ~a" sym))]))
