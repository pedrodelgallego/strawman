#lang racket
(require "env.rkt")
(provide straw-eval (struct-out closure))

(struct closure (params body env) #:transparent)

;; Exception struct for throw: carries a tag and a value.
(struct exn:straw-throw (tag value) #:transparent)

;; Exception struct for return-from: carries a block name and a value.
(struct exn:straw-return-from (name value) #:transparent)

;; straw-eval : s-expr × env [× continuation] -> value
;; When called with 2 args, uses identity continuation for backward compat.
;; When called with 3 args, threads the continuation through evaluation.
(define (straw-eval expr env [k values])
  (with-handlers
    ([exn:straw-throw?
      (lambda (t)
        (error (format "no matching catch for tag: ~a" (exn:straw-throw-tag t))))]
     [exn:straw-return-from?
      (lambda (r)
        (error (format "unknown block name: ~a" (exn:straw-return-from-name r))))])
    (straw-eval/k expr env k)))

;; eval-body : (listof s-expr) × env × continuation -> value
;; Evaluate a sequence of expressions (implicit begin), passing the last
;; result to the continuation.
(define (eval-body exprs env k)
  (cond
    [(null? exprs) (k (void))]
    [(null? (cdr exprs)) (straw-eval/k (car exprs) env k)]
    [else (straw-eval/k (car exprs) env
                        (lambda (_) (eval-body (cdr exprs) env k)))]))

;; eval-list : (listof s-expr) × env × ((listof value) -> value) -> value
;; Evaluate a list of expressions left to right, collecting results,
;; then pass the list of values to the continuation.
(define (eval-list exprs env k)
  (if (null? exprs)
      (k '())
      (straw-eval/k (car exprs) env
                     (lambda (v)
                       (eval-list (cdr exprs) env
                                  (lambda (vs) (k (cons v vs))))))))

;; straw-eval/k : s-expr × env × continuation -> value
;; The core CPS evaluator. Every result goes through k.
(define (straw-eval/k expr env k)
  (cond
    [(number? expr) (k expr)]
    [(string? expr) (k expr)]
    [(boolean? expr) (k expr)]
    [(symbol? expr) (k (env-lookup env expr))]
    [(pair? expr)
     (match expr
       [(list 'quote) (error "quote expects exactly one argument")]
       [(list 'quote datum) (k datum)]
       [(list* 'quote _ _) (error "quote expects exactly one argument")]
       [(list 'if) (error "if expects 2 or 3 arguments")]
       [(list 'if _) (error "if expects 2 or 3 arguments")]
       [(list 'if test consequent alternative)
        (straw-eval/k test env
                      (lambda (tv)
                        (if (not (eq? tv #f))
                            (straw-eval/k consequent env k)
                            (straw-eval/k alternative env k))))]
       [(list 'if test consequent)
        (straw-eval/k test env
                      (lambda (tv)
                        (if (not (eq? tv #f))
                            (straw-eval/k consequent env k)
                            (k (void)))))]
       [(list* 'if _ _) (error "if expects 2 or 3 arguments")]
       [(cons 'define (cons (cons (? symbol? name) params) body))
        #:when (andmap symbol? params)
        (env-set! env name (closure params body env))
        (k (void))]
       [(list 'define (? symbol? name) val-expr)
        (straw-eval/k val-expr env
                      (lambda (v)
                        (env-set! env name v)
                        (k (void))))]
       [(list 'set! (? symbol? name) val-expr)
        (straw-eval/k val-expr env
                      (lambda (v)
                        (env-update! env name v)
                        (k (void))))]
       [(cons 'begin body*)
        (eval-body body* env k)]
       [(cons 'lambda (list (? (lambda (p) (and (list? p) (andmap symbol? p))) params) body ...))
        (k (closure params body env))]
       [(cons 'lambda _)
        (error "expected parameter list")]
       [(cons 'let (list bindings body ...))
        (unless (list? bindings)
          (error "malformed let"))
        (for ([b (in-list bindings)])
          (unless (and (list? b) (= (length b) 2) (symbol? (car b)))
            (error "malformed binding")))
        (define names (map car bindings))
        (define init-exprs (map cadr bindings))
        (eval-list init-exprs env
                   (lambda (vals)
                     (define let-env (env-extend env names vals))
                     (eval-body body let-env k)))]
       [(cons 'let* (list bindings body ...))
        (unless (list? bindings)
          (error "malformed let*"))
        (for ([b (in-list bindings)])
          (unless (and (list? b) (= (length b) 2) (symbol? (car b)))
            (error "malformed binding")))
        (let eval-let*-bindings ([bs bindings] [e env])
          (if (null? bs)
              (eval-body body e k)
              (straw-eval/k (cadr (car bs)) e
                            (lambda (v)
                              (eval-let*-bindings
                               (cdr bs)
                               (env-extend e (list (car (car bs))) (list v)))))))]
       [(cons 'letrec (list bindings body ...))
        (unless (list? bindings)
          (error "malformed letrec"))
        (for ([b (in-list bindings)])
          (unless (and (list? b) (= (length b) 2) (symbol? (car b)))
            (error "malformed binding")))
        (define names (map car bindings))
        (define letrec-env (env-extend env names (make-list (length names) (void))))
        (let eval-letrec-bindings ([bs bindings])
          (if (null? bs)
              (eval-body body letrec-env k)
              (straw-eval/k (cadr (car bs)) letrec-env
                            (lambda (v)
                              (env-set! letrec-env (car (car bs)) v)
                              (eval-letrec-bindings (cdr bs))))))]
       [(cons 'and exprs)
        (let loop ([es exprs])
          (cond
            [(null? es) (k #t)]
            [(null? (cdr es)) (straw-eval/k (car es) env k)]
            [else
             (straw-eval/k (car es) env
                           (lambda (val)
                             (if (eq? val #f) (k #f) (loop (cdr es)))))]))]
       [(cons 'or exprs)
        (let loop ([es exprs])
          (cond
            [(null? es) (k #f)]
            [(null? (cdr es)) (straw-eval/k (car es) env k)]
            [else
             (straw-eval/k (car es) env
                           (lambda (val)
                             (if (not (eq? val #f)) (k val) (loop (cdr es)))))]))]
       ;; catch: (catch tag-expr body) — evaluate body; if throw with matching tag, return its value
       [(list 'catch tag-expr body-expr)
        (straw-eval/k tag-expr env
                      (lambda (tag)
                        (with-handlers
                          ([exn:straw-throw?
                            (lambda (t)
                              (if (equal? (exn:straw-throw-tag t) tag)
                                  (k (exn:straw-throw-value t))
                                  (raise t)))])
                          (straw-eval/k body-expr env k))))]
       ;; throw: (throw tag-expr value-expr) — unwind to nearest matching catch
       [(list 'throw tag-expr val-expr)
        (straw-eval/k tag-expr env
                      (lambda (tag)
                        (straw-eval/k val-expr env
                                      (lambda (val)
                                        (raise (exn:straw-throw tag val))))))]
       ;; call/cc: capture the current continuation
       [(list 'call/cc proc-expr)
        (straw-eval/k proc-expr env
                      (lambda (proc)
                        (cond
                          [(or (closure? proc) (procedure? proc))
                           ;; Use Racket's call/cc to get a re-entrant continuation.
                           ;; When k-proc is invoked (now or later), it jumps here
                           ;; and delivers (k v) — resuming the Strawman continuation.
                           (call-with-current-continuation
                            (lambda (racket-k)
                              (define k-proc
                                (lambda (v)
                                  (racket-k (k v))))
                              (if (closure? proc)
                                  (let ([call-env (env-extend (closure-env proc)
                                                              (closure-params proc)
                                                              (list k-proc))])
                                    (eval-body (closure-body proc) call-env k))
                                  (k (proc k-proc)))))]
                          [else (error "call/cc: expected procedure")])))]
       ;; block: (block name body ...) — evaluate body; return-from jumps out
       [(cons 'block (cons (? symbol? name) body))
        (with-handlers
          ([exn:straw-return-from?
            (lambda (r)
              (if (eq? (exn:straw-return-from-name r) name)
                  (k (exn:straw-return-from-value r))
                  (raise r)))])
          (eval-body body env k))]
       ;; unwind-protect: (unwind-protect body cleanup ...) — cleanup always runs
       [(cons 'unwind-protect (cons body-expr cleanup-exprs))
        (with-handlers
          ([(lambda (_) #t)
            (lambda (exn)
              ;; Non-local exit: run cleanup, then re-raise
              (eval-body cleanup-exprs env
                         (lambda (_) (raise exn))))])
          (straw-eval/k body-expr env
                        (lambda (v)
                          ;; Normal exit: run cleanup, then return body value
                          (eval-body cleanup-exprs env
                                     (lambda (_) (k v))))))]
       ;; return-from: (return-from name value) — exit from named block
       [(list 'return-from (? symbol? name) val-expr)
        (straw-eval/k val-expr env
                      (lambda (v)
                        (raise (exn:straw-return-from name v))))]
       ;; Function application
       [(cons func-expr arg-exprs)
        (straw-eval/k func-expr env
                      (lambda (func)
                        (eval-list arg-exprs env
                                   (lambda (args)
                                     (cond
                                       [(closure? func)
                                        (define call-env (env-extend (closure-env func)
                                                                     (closure-params func)
                                                                     args))
                                        (eval-body (closure-body func) call-env k)]
                                       [(procedure? func) (k (apply func args))]
                                       [else (error (format "not a procedure: ~a" func))])))))])]))
