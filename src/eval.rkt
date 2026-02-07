#lang racket
(require "env.rkt")
(provide straw-eval closure)

(struct closure (params body env) #:transparent)

;; straw-eval : s-expr × env -> value
(define (straw-eval expr env)
  (cond
    [(number? expr) expr]
    [(string? expr) expr]
    [(boolean? expr) expr]
    [(symbol? expr) (env-lookup env expr)]
    [(pair? expr)
     (match expr
       [(list 'quote) (error "quote expects exactly one argument")]
       [(list 'quote datum) datum]
       [(list* 'quote _ _) (error "quote expects exactly one argument")]
       [(list 'if) (error "if expects 2 or 3 arguments")]
       [(list 'if _) (error "if expects 2 or 3 arguments")]
       [(list 'if test consequent alternative)
        (if (not (eq? (straw-eval test env) #f))
            (straw-eval consequent env)
            (straw-eval alternative env))]
       [(list 'if test consequent)
        (if (not (eq? (straw-eval test env) #f))
            (straw-eval consequent env)
            (void))]
       [(list* 'if _ _) (error "if expects 2 or 3 arguments")]
       [(list 'define (? symbol? name) val-expr)
        (env-set! env name (straw-eval val-expr env))
        (void)]
       [(list 'set! (? symbol? name) val-expr)
        (env-update! env name (straw-eval val-expr env))
        (void)]
       [(cons 'begin body*)
        (let loop ([exprs body*])
          (cond
            [(null? exprs) (void)]
            [(null? (cdr exprs)) (straw-eval (car exprs) env)]
            [else (straw-eval (car exprs) env)
                  (loop (cdr exprs))]))]
       [(cons 'lambda (list (? (lambda (p) (and (list? p) (andmap symbol? p))) params) body ...))
        (closure params body env)]
       [(cons 'lambda _)
        (error "expected parameter list")]
       ;; Function application
       [(cons func-expr arg-exprs)
        (define func (straw-eval func-expr env))
        (define args (map (lambda (a) (straw-eval a env)) arg-exprs))
        (cond
          [(closure? func)
           (define call-env (env-extend (closure-env func)
                                        (closure-params func)
                                        args))
           (let loop ([exprs (closure-body func)])
             (cond
               [(null? exprs) (void)]
               [(null? (cdr exprs)) (straw-eval (car exprs) call-env)]
               [else (straw-eval (car exprs) call-env)
                     (loop (cdr exprs))]))]
          [(procedure? func) (apply func args)]
          [else (error (format "not a procedure: ~a" func))])])]))
