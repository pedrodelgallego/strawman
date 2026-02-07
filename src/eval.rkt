#lang racket
(require "env.rkt" "objects.rkt")
(provide straw-eval (struct-out closure) (struct-out straw-macro))

(struct closure (params body env) #:transparent)

;; A macro is a transformer function: it receives unevaluated arguments
;; and returns a new form to be evaluated.
(struct straw-macro (params body env) #:transparent)

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

;; eval-quasiquote : template × depth × env × continuation -> value
;; Process a quasiquote template at a given nesting depth.
;; At depth 0, unquote evaluates and unquote-splicing splices.
;; At depth > 0, nested quasiquote/unquote are treated as data.
(define (eval-quasiquote tmpl depth env k)
  (cond
    ;; (unquote expr) at depth 0: evaluate expr
    [(and (pair? tmpl) (eq? (car tmpl) 'unquote) (= depth 0))
     (straw-eval/k (cadr tmpl) env k)]
    ;; (unquote expr) at depth > 0: decrease depth
    [(and (pair? tmpl) (eq? (car tmpl) 'unquote))
     (eval-quasiquote (cadr tmpl) (sub1 depth) env
                      (lambda (v) (k (list 'unquote v))))]
    ;; (quasiquote expr): increase depth
    [(and (pair? tmpl) (eq? (car tmpl) 'quasiquote))
     (eval-quasiquote (cadr tmpl) (add1 depth) env
                      (lambda (v) (k (list 'quasiquote v))))]
    ;; List: process elements, handling unquote-splicing
    [(pair? tmpl)
     (eval-qq-list tmpl depth env k)]
    ;; Atom: return as-is
    [else (k tmpl)]))

;; eval-qq-list : list × depth × env × continuation -> value
;; Process elements of a quasiquote list, handling splicing.
(define (eval-qq-list elems depth env k)
  (if (null? elems)
      (k '())
      (let ([head (car elems)]
            [tail (cdr elems)])
        (cond
          ;; (unquote-splicing expr) at depth 0: evaluate and splice
          [(and (pair? head) (eq? (car head) 'unquote-splicing) (= depth 0))
           (straw-eval/k (cadr head) env
                         (lambda (spliced)
                           (eval-qq-list tail depth env
                                        (lambda (rest)
                                          (k (append (mlist->immutable-list spliced) rest))))))]
          ;; Regular element: process recursively
          [else
           (eval-quasiquote head depth env
                            (lambda (v)
                              (eval-qq-list tail depth env
                                           (lambda (rest)
                                             (k (cons v rest))))))]))))

;; mlist->immutable-list : datum -> list
;; Convert a potentially mutable list (from Strawman's cons/list) to an immutable Racket list.
(define (mlist->immutable-list datum)
  (cond
    [(null? datum) '()]
    [(mpair? datum) (cons (mcar datum) (mlist->immutable-list (mcdr datum)))]
    [(pair? datum) (cons (car datum) (mlist->immutable-list (cdr datum)))]
    [else (error "unquote-splicing: expected list")]))

;; mlist->list : datum -> datum
;; Convert mutable pairs (from Strawman's cons) to immutable pairs
;; so the evaluator can process them as code.
(define (mlist->list datum)
  (cond
    [(mpair? datum) (cons (mlist->list (mcar datum))
                          (mlist->list (mcdr datum)))]
    [else datum]))

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
       ;; define-macro: (define-macro (name params ...) body ...)
       [(cons 'define-macro (cons (cons (? symbol? name) params) body))
        #:when (andmap symbol? params)
        (env-set! env name (straw-macro params body env))
        (k (void))]
       ;; macroexpand: (macroexpand '(macro-name args...)) — expand one level, don't eval
       [(list 'macroexpand form-expr)
        (straw-eval/k form-expr env
                      (lambda (form)
                        (if (and (pair? form) (symbol? (car form)))
                            (let ([maybe-macro
                                   (with-handlers ([exn:fail? (lambda (_) #f)])
                                     (env-lookup env (car form)))])
                              (if (straw-macro? maybe-macro)
                                  (let* ([macro-env (env-extend (straw-macro-env maybe-macro)
                                                                (straw-macro-params maybe-macro)
                                                                (cdr form))]
                                         [expanded (eval-body (straw-macro-body maybe-macro)
                                                              macro-env values)])
                                    (k (mlist->list expanded)))
                                  (k form)))
                            (k form))))]
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
       ;; cond: (cond (test expr) ... (else expr)) → nested if
       [(cons 'cond clauses)
        (let expand-cond ([cs clauses])
          (cond
            [(null? cs) (k (void))]
            [(and (pair? (car cs)) (eq? (caar cs) 'else))
             (eval-body (cdar cs) env k)]
            [(pair? (car cs))
             (straw-eval/k (caar cs) env
                           (lambda (tv)
                             (if (not (eq? tv #f))
                                 (eval-body (cdar cs) env k)
                                 (expand-cond (cdr cs)))))]
            [else (error "cond: bad clause")]))]
       ;; when: (when test body ...) → (if test (begin body ...) void)
       [(cons 'when (cons test body))
        (straw-eval/k test env
                      (lambda (tv)
                        (if (not (eq? tv #f))
                            (eval-body body env k)
                            (k (void)))))]
       ;; unless: (unless test body ...) → (if test void (begin body ...))
       [(cons 'unless (cons test body))
        (straw-eval/k test env
                      (lambda (tv)
                        (if (eq? tv #f)
                            (eval-body body env k)
                            (k (void)))))]
       ;; quasiquote: template syntax with unquote and unquote-splicing
       [(list 'quasiquote template)
        (eval-quasiquote template 0 env k)]
       ;; the-environment: returns the current environment as a first-class value
       [(list 'the-environment)
        (k env)]
       ;; define-class: (define-class Name (Parent) (field1 field2 ...))
       [(list 'define-class (? symbol? name) (list parent-name ...) (list field-names ...))
        ;; Validate field names are symbols
        (unless (andmap symbol? field-names)
          (error "define-class: field names must be symbols"))
        ;; Check for duplicate fields in own fields
        (let ([seen (make-hash)])
          (for ([f (in-list field-names)])
            (when (hash-has-key? seen f)
              (error (format "duplicate field: ~a" f)))
            (hash-set! seen f #t)))
        ;; Resolve parent class
        (define parent
          (cond
            [(null? parent-name) #f]
            [else
             (define pname (car parent-name))
             (unless (symbol? pname)
               (error "define-class: parent must be a symbol"))
             (define p
               (with-handlers ([exn:fail? (lambda (_) #f)])
                 (env-lookup env pname)))
             (unless (straw-class? p)
               (error (format "unknown parent class: ~a" pname)))
             p]))
        (define cls (straw-class name parent field-names))
        (env-set! env name cls)
        ;; Auto-generate constructor, accessors, mutators, predicate
        (define all-fields (straw-class-all-fields cls))
        (define n-fields (length all-fields))
        (define class-name-str (symbol->string name))
        ;; Constructor: make-<Name>
        (env-set! env (string->symbol (string-append "make-" class-name-str))
                  (lambda args
                    (unless (= (length args) n-fields)
                      (error (format "arity mismatch: expected ~a, got ~a"
                                     n-fields (length args))))
                    (straw-instance cls (list->vector args))))
        ;; Predicate: <Name>?
        (env-set! env (string->symbol (string-append class-name-str "?"))
                  (lambda (v)
                    (and (straw-instance? v)
                         (straw-class-is-a? (straw-instance-class v) cls))))
        ;; Accessors and mutators for each field
        (for ([field (in-list all-fields)]
              [i (in-naturals)])
          (define field-str (symbol->string field))
          ;; Accessor: <Name>-<field>
          (env-set! env (string->symbol (string-append class-name-str "-" field-str))
                    (lambda (obj)
                      (unless (and (straw-instance? obj)
                                   (straw-class-is-a? (straw-instance-class obj) cls))
                        (error (format "expected ~a" name)))
                      (vector-ref (straw-instance-field-values obj) i)))
          ;; Mutator: set-<Name>-<field>!
          (env-set! env (string->symbol (string-append "set-" class-name-str "-" field-str "!"))
                    (lambda (obj val)
                      (unless (and (straw-instance? obj)
                                   (straw-class-is-a? (straw-instance-class obj) cls))
                        (error (format "expected ~a" name)))
                      (vector-set! (straw-instance-field-values obj) i val)
                      (void))))
        (k (void))]
       ;; define-generic: (define-generic name) — create a generic function
       [(list 'define-generic (? symbol? name))
        (env-set! env name (straw-generic name (make-hash)))
        (k (void))]
       ;; define-method: (define-method (generic-name (self ClassName) extra-params ...) body ...)
       [(cons 'define-method (cons (cons (? symbol? gname) (cons (list (? symbol? self-name) (? symbol? class-name)) extra-params)) body))
        (define gen (env-lookup env gname))
        (unless (straw-generic? gen)
          (error (format "define-method: ~a is not a generic" gname)))
        (define cls (env-lookup env class-name))
        (unless (straw-class? cls)
          (error (format "define-method: ~a is not a class" class-name)))
        ;; Build a closure that takes (self extra-params...)
        (define all-params (cons self-name extra-params))
        (define method (closure all-params body env))
        (hash-set! (straw-generic-methods gen) cls method)
        (k (void))]
       ;; eval: (eval expr) or (eval expr env) — evaluate expr as code
       [(list 'eval expr)
        (straw-eval/k expr env
                      (lambda (datum)
                        (straw-eval/k (mlist->list datum) env k)))]
       [(list 'eval expr env-expr)
        (straw-eval/k expr env
                      (lambda (datum)
                        (straw-eval/k env-expr env
                                      (lambda (eval-env)
                                        (straw-eval/k (mlist->list datum) eval-env k)))))]
       ;; Function application (check for macro first)
       [(cons func-expr arg-exprs)
        #:when (and (symbol? func-expr)
                    (let ([v (with-handlers ([exn:fail? (lambda (_) #f)])
                               (env-lookup env func-expr))])
                      (straw-macro? v)))
        ;; Macro application: call transformer with unevaluated args, then eval result
        (let* ([mac (env-lookup env func-expr)]
               [macro-env (env-extend (straw-macro-env mac)
                                      (straw-macro-params mac)
                                      arg-exprs)]
               [expanded (eval-body (straw-macro-body mac) macro-env values)])
          (straw-eval/k (mlist->list expanded) env k))]
       ;; Regular function application
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
                                       [(straw-generic? func)
                                        (define method (straw-generic-dispatch func args))
                                        (define call-env (env-extend (closure-env method)
                                                                     (closure-params method)
                                                                     args))
                                        (eval-body (closure-body method) call-env k)]
                                       [else (error (format "not a procedure: ~a" func))])))))])]))
