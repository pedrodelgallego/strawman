#lang racket
(require "env.rkt")
(provide straw-eval (struct-out closure))

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
       [(cons 'define (cons (cons (? symbol? name) params) body))
        #:when (andmap symbol? params)
        (env-set! env name (closure params body env))
        (void)]
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
       [(cons 'let (list bindings body ...))
        (unless (list? bindings)
          (error "malformed let"))
        (for ([b (in-list bindings)])
          (unless (and (list? b) (= (length b) 2) (symbol? (car b)))
            (error "malformed binding")))
        (define names (map car bindings))
        (define vals (map (lambda (b) (straw-eval (cadr b) env)) bindings))
        (define let-env (env-extend env names vals))
        (let loop ([exprs body])
          (cond
            [(null? exprs) (void)]
            [(null? (cdr exprs)) (straw-eval (car exprs) let-env)]
            [else (straw-eval (car exprs) let-env)
                  (loop (cdr exprs))]))]
       [(cons 'let* (list bindings body ...))
        (unless (list? bindings)
          (error "malformed let*"))
        (for ([b (in-list bindings)])
          (unless (and (list? b) (= (length b) 2) (symbol? (car b)))
            (error "malformed binding")))
        (define let*-env
          (for/fold ([e env]) ([b (in-list bindings)])
            (env-extend e (list (car b)) (list (straw-eval (cadr b) e)))))
        (let loop ([exprs body])
          (cond
            [(null? exprs) (void)]
            [(null? (cdr exprs)) (straw-eval (car exprs) let*-env)]
            [else (straw-eval (car exprs) let*-env)
                  (loop (cdr exprs))]))]
       [(cons 'letrec (list bindings body ...))
        (unless (list? bindings)
          (error "malformed letrec"))
        (for ([b (in-list bindings)])
          (unless (and (list? b) (= (length b) 2) (symbol? (car b)))
            (error "malformed binding")))
        (define names (map car bindings))
        (define letrec-env (env-extend env names (make-list (length names) (void))))
        (for ([b (in-list bindings)])
          (env-set! letrec-env (car b) (straw-eval (cadr b) letrec-env)))
        (let loop ([exprs body])
          (cond
            [(null? exprs) (void)]
            [(null? (cdr exprs)) (straw-eval (car exprs) letrec-env)]
            [else (straw-eval (car exprs) letrec-env)
                  (loop (cdr exprs))]))]
       [(cons 'and exprs)
        (let loop ([es exprs])
          (cond
            [(null? es) #t]
            [(null? (cdr es)) (straw-eval (car es) env)]
            [else
             (define val (straw-eval (car es) env))
             (if (eq? val #f) #f (loop (cdr es)))]))]
       [(cons 'or exprs)
        (let loop ([es exprs])
          (cond
            [(null? es) #f]
            [(null? (cdr es)) (straw-eval (car es) env)]
            [else
             (define val (straw-eval (car es) env))
             (if (not (eq? val #f)) val (loop (cdr es)))]))]
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
