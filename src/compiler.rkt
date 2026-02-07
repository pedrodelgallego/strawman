#lang racket
(provide compile)

;; compile : s-expr -> (listof instruction)
;; Translates a Strawman S-expression into a flat list of bytecode instructions.
;; Each instruction is a list: (OPCODE operand ...).
;; All jump targets are absolute indices into the output list.

(define (compile expr)
  (compile-expr expr 0))

;; compile-expr : s-expr × nat -> (listof instruction)
;; Core compilation dispatch. base is the absolute offset at which this block starts.
(define (compile-expr expr base)
  (cond
    [(number? expr)  (list (list 'CONST expr))]
    [(string? expr)  (list (list 'CONST expr))]
    [(boolean? expr) (list (list 'CONST expr))]
    [(symbol? expr)  (list (list 'LOOKUP expr))]
    [(pair? expr)    (compile-form expr base)]
    [else (error (format "compile: unknown expression: ~a" expr))]))

;; compile-form : pair × nat -> (listof instruction)
;; Compile a compound form (special form or application).
(define (compile-form expr base)
  (match expr
    [(list 'quote datum)
     (list (list 'CONST datum))]

    [(list 'if test consequent alternative)
     (compile-if test consequent alternative base)]

    [(list 'if test consequent)
     (compile-if test consequent #f base)]

    [(cons 'begin body)
     (compile-begin body base)]

    ;; (define (f params ...) body ...) → (define f (lambda (params ...) body ...))
    [(cons 'define (cons (cons (? symbol? name) params) body))
     (define lambda-code (compile-expr (cons 'lambda (cons params body)) base))
     (append lambda-code
             (list (list 'DEFINE name)))]

    [(list 'define (? symbol? name) val-expr)
     (define val-code (compile-expr val-expr base))
     (append val-code
             (list (list 'DEFINE name)))]

    [(list 'set! (? symbol? name) val-expr)
     (define val-code (compile-expr val-expr base))
     (append val-code
             (list (list 'SET name)))]

    [(cons 'lambda (cons (? list? params) body))
     (compile-lambda params body base)]

    [(cons 'and exprs)
     (compile-and exprs base)]

    [(cons 'or exprs)
     (compile-or exprs base)]

    [(cons 'let (cons (? list? bindings) body))
     (compile-let bindings body base)]

    [(cons 'let* (cons (? list? bindings) body))
     (compile-let* bindings body base)]

    [(cons 'letrec (cons (? list? bindings) body))
     (compile-letrec bindings body base)]

    ;; Function application: (f arg1 arg2 ...)
    [(cons func-expr arg-exprs)
     (define func-code (compile-expr func-expr base))
     (define-values (arg-codes _)
       (for/fold ([codes '()] [off (+ base (length func-code))])
                 ([a (in-list arg-exprs)])
         (define c (compile-expr a off))
         (values (append codes c) (+ off (length c)))))
     (append func-code
             arg-codes
             (list (list 'CALL (length arg-exprs))))]))

;; compile-if : s-expr × s-expr × (or s-expr #f) × nat -> (listof instruction)
(define (compile-if test consequent alternative base)
  (define test-code (compile-expr test base))
  (define test-len (length test-code))
  ;; JIF is at base + test-len
  (define cons-code (compile-expr consequent (+ base test-len 1)))
  (define cons-len (length cons-code))
  ;; JUMP is at base + test-len + 1 + cons-len
  (define alt-start (+ base test-len 1 cons-len 1))
  (define alt-code (if alternative
                       (compile-expr alternative alt-start)
                       (list (list 'CONST (void)))))
  (define end (+ alt-start (length alt-code)))
  (append test-code
          (list (list 'JUMP-IF-FALSE alt-start))
          cons-code
          (list (list 'JUMP end))
          alt-code))

;; compile-begin : (listof s-expr) × nat -> (listof instruction)
(define (compile-begin exprs base)
  (cond
    [(null? exprs) (list (list 'CONST (void)))]
    [(null? (cdr exprs)) (compile-expr (car exprs) base)]
    [else
     (define first-code (compile-expr (car exprs) base))
     (define rest-base (+ base (length first-code) 1)) ; +1 for POP
     (define rest-code (compile-begin (cdr exprs) rest-base))
     (append first-code
             (list '(POP))
             rest-code)]))

;; compile-lambda : (listof symbol) × (listof s-expr) × nat -> (listof instruction)
(define (compile-lambda params body base)
  ;; Layout:
  ;;   CLOSURE body-start params     ← base
  ;;   JUMP after-body               ← base+1
  ;;   <body code>                   ← body-start = base+2
  ;;   RETURN
  ;;                                 ← after-body
  (define body-start (+ base 2))
  (define body-code (compile-begin body body-start))
  (define after-body (+ body-start (length body-code) 1))  ; +1 for RETURN
  (append (list (list 'CLOSURE body-start params))
          (list (list 'JUMP after-body))
          body-code
          (list '(RETURN))))

;; compile-and : (listof s-expr) × nat -> (listof instruction)
(define (compile-and exprs base)
  (cond
    [(null? exprs) (list (list 'CONST #t))]
    [(null? (cdr exprs)) (compile-expr (car exprs) base)]
    [else
     (define init-exprs (reverse (cdr (reverse exprs))))
     (define last-expr (car (reverse exprs)))
     ;; Two-pass: first compute all code blocks to know sizes,
     ;; then rebuild with absolute offsets.
     ;; Pass 1: compile with dummy base to get sizes
     (define init-codes-sizes
       (for/list ([e (in-list init-exprs)])
         (define c (compile-expr e 0))
         (length c)))
     (define last-code-size
       (length (compile-expr last-expr 0)))
     ;; Calculate total sizes
     (define init-total
       (for/sum ([s (in-list init-codes-sizes)])
         (+ s 1)))  ; each block + JIF
     (define lend (+ base init-total last-code-size 1))  ; after last-code + JUMP
     (define ldone (+ lend 1))  ; after CONST #f
     ;; Pass 2: compile with correct bases
     (define-values (init-instrs curr-base)
       (for/fold ([instrs '()] [off base])
                 ([e (in-list init-exprs)])
         (define c (compile-expr e off))
         (values (append instrs c (list (list 'JUMP-IF-FALSE lend)))
                 (+ off (length c) 1))))
     (define last-code (compile-expr last-expr curr-base))
     (append init-instrs
             last-code
             (list (list 'JUMP ldone))
             (list (list 'CONST #f)))]))

;; compile-or : (listof s-expr) × nat -> (listof instruction)
;; Desugars (or e1 e2 ... en) to nested let/if to preserve truthy values:
;;   (let ((t e1)) (if t t (or e2 ... en)))
(define (compile-or exprs base)
  (cond
    [(null? exprs) (list (list 'CONST #f))]
    [(null? (cdr exprs)) (compile-expr (car exprs) base)]
    [else
     ;; Generate a fresh temp variable name to avoid collisions
     (define temp (gensym 'or-tmp))
     (define desugared
       `(let ((,temp ,(car exprs)))
          (if ,temp ,temp (or ,@(cdr exprs)))))
     (compile-expr desugared base)]))

;; compile-let : bindings × body × nat -> (listof instruction)
;; Compiles as application of anonymous lambda: ((lambda (x1 x2 ...) body) e1 e2 ...)
;; Order: push lambda (proc) first, then init values (args), then CALL n
(define (compile-let bindings body base)
  (define names (map car bindings))
  (define init-exprs (map cadr bindings))
  ;; Compile the lambda for the body first (proc pushed first)
  (define lambda-code (compile-lambda names body base))
  (define after-lambda (+ base (length lambda-code)))
  ;; Compile init expressions
  (define-values (init-code init-end)
    (for/fold ([codes '()] [off after-lambda])
              ([e (in-list init-exprs)])
      (define c (compile-expr e off))
      (values (append codes c) (+ off (length c)))))
  ;; Layout: lambda-code, init-code, CALL n
  (append lambda-code
          init-code
          (list (list 'CALL (length bindings)))))

;; compile-let* : bindings × body × nat -> (listof instruction)
;; Desugars to nested lets
(define (compile-let* bindings body base)
  (if (null? bindings)
      (compile-begin body base)
      (compile-let (list (car bindings))
                   (list (cons 'let* (cons (cdr bindings) body)))
                   base)))

;; compile-letrec : bindings × body × nat -> (listof instruction)
(define (compile-letrec bindings body base)
  (define names (map car bindings))
  (define init-exprs (map cadr bindings))
  ;; CONST void, DEFINE x1, CONST void, DEFINE x2, ...
  (define void-defs
    (append-map (lambda (name)
                  (list (list 'CONST (void)) (list 'DEFINE name)))
                names))
  (define after-defs (+ base (length void-defs)))
  ;; compile e1, SET x1, compile e2, SET x2, ...
  (define-values (init-sets sets-end)
    (for/fold ([codes '()] [off after-defs])
              ([name (in-list names)]
               [init (in-list init-exprs)])
      (define c (compile-expr init off))
      (values (append codes c (list (list 'SET name)))
              (+ off (length c) 1))))
  (define body-code (compile-begin body sets-end))
  (append void-defs init-sets body-code))
