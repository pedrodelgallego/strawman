#lang racket
(require "env.rkt")
(provide pretreat
         (struct-out treated-const)
         (struct-out treated-ref)
         (struct-out treated-lexical-ref)
         (struct-out treated-quote)
         (struct-out treated-if)
         (struct-out treated-define)
         (struct-out treated-define-fun)
         (struct-out treated-set!)
         (struct-out treated-begin)
         (struct-out treated-lambda)
         (struct-out treated-let)
         (struct-out treated-let*)
         (struct-out treated-letrec)
         (struct-out treated-and)
         (struct-out treated-or)
         (struct-out treated-catch)
         (struct-out treated-throw)
         (struct-out treated-call/cc)
         (struct-out treated-block)
         (struct-out treated-unwind-protect)
         (struct-out treated-return-from)
         (struct-out treated-app))

;; Treated expression structs — the pretreatment pass converts raw s-expressions
;; into these, so the evaluator can dispatch on struct type instead of matching
;; against lists of symbols.

(struct treated-const (value) #:transparent)
(struct treated-ref (name) #:transparent)
(struct treated-lexical-ref (depth offset) #:transparent)
(struct treated-quote (datum) #:transparent)
(struct treated-if (test consequent alternative) #:transparent)
(struct treated-define (name value-expr) #:transparent)
(struct treated-define-fun (name params body) #:transparent)
(struct treated-set! (name value-expr) #:transparent)
(struct treated-begin (exprs) #:transparent)
(struct treated-lambda (params body) #:transparent)
(struct treated-let (bindings body) #:transparent)        ; bindings = list of (name . treated-expr)
(struct treated-let* (bindings body) #:transparent)
(struct treated-letrec (bindings body) #:transparent)
(struct treated-and (exprs) #:transparent)
(struct treated-or (exprs) #:transparent)
(struct treated-catch (tag-expr body-expr) #:transparent)
(struct treated-throw (tag-expr value-expr) #:transparent)
(struct treated-call/cc (proc-expr) #:transparent)
(struct treated-block (name body) #:transparent)
(struct treated-unwind-protect (body-expr cleanup-exprs) #:transparent)
(struct treated-return-from (name value-expr) #:transparent)
(struct treated-app (func-expr arg-exprs) #:transparent)

;; static-env-lookup : symbol × (listof (listof symbol)) -> (cons nat nat) or #f
;; Search the static environment (list of ribs) for a symbol.
;; Returns (cons depth offset) if found, #f otherwise.
(define (static-env-lookup sym senv)
  (let loop ([ribs senv] [depth 0])
    (cond
      [(null? ribs) #f]
      [else
       (define rib (car ribs))
       (define offset (index-of rib sym))
       (if offset
           (cons depth offset)
           (loop (cdr ribs) (+ depth 1)))])))

;; pretreat : s-expr -> treated-expr
;; Public API — delegates to pretreat/senv with an empty static environment.
(define (pretreat expr)
  (pretreat/senv expr '()))

;; pretreat/senv : s-expr × (listof (listof symbol)) -> treated-expr
;; Analyze an s-expression with a static environment for lexical addressing.
(define (pretreat/senv expr senv)
  (define (pt e) (pretreat/senv e senv))
  (define (pt* es) (map pt es))
  (cond
    [(number? expr)  (treated-const expr)]
    [(string? expr)  (treated-const expr)]
    [(boolean? expr) (treated-const expr)]
    [(symbol? expr)
     (define result (static-env-lookup expr senv))
     (if result
         (treated-lexical-ref (car result) (cdr result))
         (treated-ref expr))]
    [(pair? expr)
     (match expr
       [(list 'quote)          (error "quote expects exactly one argument")]
       [(list 'quote datum)    (treated-quote datum)]
       [(list* 'quote _ _)     (error "quote expects exactly one argument")]

       [(list 'if)             (error "if expects 2 or 3 arguments")]
       [(list 'if _)           (error "if expects 2 or 3 arguments")]
       [(list 'if test c a)    (treated-if (pt test) (pt c) (pt a))]
       [(list 'if test c)      (treated-if (pt test) (pt c) (treated-const (void)))]
       [(list* 'if _ _)        (error "if expects 2 or 3 arguments")]

       [(cons 'define (cons (cons (? symbol? name) params) body))
        #:when (andmap symbol? params)
        (define body-senv (cons params senv))
        (treated-define-fun name params (map (lambda (e) (pretreat/senv e body-senv)) body))]
       [(list 'define (? symbol? name) val-expr)
        (treated-define name (pt val-expr))]

       [(list 'set! (? symbol? name) val-expr)
        (treated-set! name (pt val-expr))]

       [(cons 'begin body*)
        (treated-begin (pt* body*))]

       [(cons 'lambda (list (? (lambda (p) (and (list? p) (andmap symbol? p))) params) body ...))
        (define body-senv (cons params senv))
        (treated-lambda params (map (lambda (e) (pretreat/senv e body-senv)) body))]
       [(cons 'lambda _)
        (error "expected parameter list")]

       [(cons 'let (list bindings body ...))
        (unless (list? bindings)
          (error "malformed let"))
        (for ([b (in-list bindings)])
          (unless (and (list? b) (= (length b) 2) (symbol? (car b)))
            (error "malformed binding")))
        (define names (map car bindings))
        (define body-senv (cons names senv))
        (treated-let (map (lambda (b) (cons (car b) (pt (cadr b)))) bindings)
                     (map (lambda (e) (pretreat/senv e body-senv)) body))]

       [(cons 'let* (list bindings body ...))
        (unless (list? bindings)
          (error "malformed let*"))
        (for ([b (in-list bindings)])
          (unless (and (list? b) (= (length b) 2) (symbol? (car b)))
            (error "malformed binding")))
        ;; For let*, each binding extends the senv for subsequent bindings and body
        (define-values (treated-bindings final-senv)
          (for/fold ([tbs '()] [se senv])
                    ([b (in-list bindings)])
            (define tb (cons (car b) (pretreat/senv (cadr b) se)))
            (values (append tbs (list tb))
                    (cons (list (car b)) se))))
        (treated-let* treated-bindings
                      (map (lambda (e) (pretreat/senv e final-senv)) body))]

       [(cons 'letrec (list bindings body ...))
        (unless (list? bindings)
          (error "malformed letrec"))
        (for ([b (in-list bindings)])
          (unless (and (list? b) (= (length b) 2) (symbol? (car b)))
            (error "malformed binding")))
        (define names (map car bindings))
        (define body-senv (cons names senv))
        (treated-letrec (map (lambda (b) (cons (car b) (pretreat/senv (cadr b) body-senv))) bindings)
                        (map (lambda (e) (pretreat/senv e body-senv)) body))]

       [(cons 'and exprs)
        (treated-and (pt* exprs))]

       [(cons 'or exprs)
        (treated-or (pt* exprs))]

       [(list 'catch tag-expr body-expr)
        (treated-catch (pt tag-expr) (pt body-expr))]

       [(list 'throw tag-expr val-expr)
        (treated-throw (pt tag-expr) (pt val-expr))]

       [(list 'call/cc proc-expr)
        (treated-call/cc (pt proc-expr))]

       [(cons 'block (cons (? symbol? name) body))
        (treated-block name (pt* body))]

       [(cons 'unwind-protect (cons body-expr cleanup-exprs))
        (treated-unwind-protect (pt body-expr) (pt* cleanup-exprs))]

       [(list 'return-from (? symbol? name) val-expr)
        (treated-return-from name (pt val-expr))]

       ;; Function application
       [(cons func-expr arg-exprs)
        (treated-app (pt func-expr) (pt* arg-exprs))])]
    [else (error (format "pretreat: unknown expression: ~a" expr))]))
