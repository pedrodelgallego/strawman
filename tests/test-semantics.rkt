#lang racket
(require rackunit
         "../src/eval.rkt"
         "../src/builtins.rkt")

;; Helper: evaluate an expression in the default environment
(define (ev expr)
  (straw-eval expr (default-env)))

;; E5.1 — Tests that docs/semantics.md exists and covers all required
;; denotational semantics domains and valuation functions.

;; Resolve relative to this file's directory (tests/), going up one level to project root
(define semantics-path
  (build-path (path-only (syntax-source #'here)) ".." "docs" "semantics.md"))

(define (file-contents path)
  (file->string path))

;; The document must exist
(test-case "docs/semantics.md exists"
  (check-true (file-exists? semantics-path)
              "docs/semantics.md should exist"))

;; Semantic domains required by spec: Env, Value, Cont, Mem
(test-case "defines semantic domain Env"
  (check-regexp-match #rx"Env" (file-contents semantics-path)))

(test-case "defines semantic domain Value"
  (check-regexp-match #rx"Value" (file-contents semantics-path)))

(test-case "defines semantic domain Cont"
  (check-regexp-match #rx"Cont" (file-contents semantics-path)))

(test-case "defines semantic domain Mem"
  (check-regexp-match #rx"Mem" (file-contents semantics-path)))

;; Valuation functions for each core form
(test-case "valuation for numbers"
  (check-regexp-match #rx"number" (file-contents semantics-path)))

(test-case "valuation for symbols"
  (check-regexp-match #rx"symbol" (file-contents semantics-path)))

(test-case "valuation for quote"
  (check-regexp-match #rx"quote" (file-contents semantics-path)))

(test-case "valuation for if"
  (check-regexp-match #rx"[(]if " (file-contents semantics-path)))

(test-case "valuation for begin"
  (check-regexp-match #rx"begin" (file-contents semantics-path)))

(test-case "valuation for define"
  (check-regexp-match #rx"define" (file-contents semantics-path)))

(test-case "valuation for set!"
  (check-regexp-match #rx"set!" (file-contents semantics-path)))

(test-case "valuation for lambda"
  (check-regexp-match #rx"lambda" (file-contents semantics-path)))

(test-case "valuation for application"
  (check-regexp-match #rx"[Aa]ppli" (file-contents semantics-path)))

;; =========================================================================
;; E5.2 — Semantics-derived evaluator tests
;;
;; One test per valuation clause in docs/semantics.md. Each test-case name
;; references the clause it validates: "§3.N description".
;; =========================================================================

;; --- §3.1 Self-evaluating: number, string, boolean ---
;; E[[n]] env mem cont = cont(n, mem)
(test-case "§3.1 number literal evaluates to itself"
  (check-equal? (ev 42) 42))

;; E[[s]] env mem cont = cont(s, mem)
(test-case "§3.1 string literal evaluates to itself"
  (check-equal? (ev "hello") "hello"))

;; E[[b]] env mem cont = cont(b, mem)
(test-case "§3.1 boolean #t evaluates to itself"
  (check-equal? (ev #t) #t))

(test-case "§3.1 boolean #f evaluates to itself"
  (check-equal? (ev #f) #f))

;; --- §3.2 Symbol lookup ---
;; E[[x]] env mem cont = cont(mem(env(x)), mem)
(test-case "§3.2 symbol lookup retrieves bound value"
  (check-equal? (ev '(let ((x 7)) x)) 7))

;; Error: unbound variable
(test-case "§3.2 unbound symbol signals error"
  (check-exn #rx"unbound variable"
             (lambda () (ev 'no-such-var))))

;; --- §3.3 Quote ---
;; E[[(quote datum)]] env mem cont = cont(datum, mem)
(test-case "§3.3 quote returns datum unevaluated"
  (check-equal? (ev '(quote (a b c))) '(a b c)))

;; --- §3.4 If (conditional) ---
;; Truthy branch: v != #f => consequent
(test-case "§3.4 if truthy takes consequent"
  (check-equal? (ev '(if 1 42 99)) 42))

;; Falsy branch: v = #f => alternative
(test-case "§3.4 if #f takes alternative"
  (check-equal? (ev '(if #f 42 99)) 99))

;; No alternative, false test => void
(test-case "§3.4 if #f without alternative yields void"
  (check-equal? (ev '(if #f 42)) (void)))

;; Truthiness: only #f is false (0, "", '() are truthy)
(test-case "§3.4/§6.5 zero is truthy"
  (check-equal? (ev '(if 0 "yes" "no")) "yes"))

;; --- §3.5 Begin (sequencing) ---
;; Empty begin => void
(test-case "§3.5 empty begin yields void"
  (check-equal? (ev '(begin)) (void)))

;; Single expression
(test-case "§3.5 begin with one expression returns its value"
  (check-equal? (ev '(begin 42)) 42))

;; Sequence: value of last expression
(test-case "§3.5 begin returns last expression's value"
  (check-equal? (ev '(begin 1 2 3)) 3))

;; Store threading: side effects from earlier expressions are visible
(test-case "§3.5 begin threads store through expressions"
  (check-equal? (ev '(let ((x 0))
                       (begin (set! x 10) x)))
                10))

;; --- §3.6 Define (binding creation) ---
;; E[[(define x expr)]] => void, binds x
(test-case "§3.6 define returns void and creates binding"
  (let ([env (default-env)])
    (check-equal? (straw-eval '(define x 5) env) (void))
    (check-equal? (straw-eval 'x env) 5)))

;; Shorthand: (define (f params) body) = (define f (lambda (params) body))
(test-case "§3.6 define shorthand creates function binding"
  (let ([env (default-env)])
    (straw-eval '(define (square n) (* n n)) env)
    (check-equal? (straw-eval '(square 4) env) 16)))

;; --- §3.7 Set! (mutation) ---
;; E[[(set! x expr)]] => void, updates existing binding
(test-case "§3.7 set! mutates existing binding"
  (let ([env (default-env)])
    (straw-eval '(define x 1) env)
    (check-equal? (straw-eval '(set! x 99) env) (void))
    (check-equal? (straw-eval 'x env) 99)))

;; Error: set! on unbound variable
(test-case "§3.7 set! on unbound variable signals error"
  (check-exn #rx"cannot set! unbound variable"
             (lambda () (ev '(set! nonexistent 1)))))

;; --- §3.8 Lambda (abstraction) ---
;; E[[(lambda (params) body)]] => Closure capturing env
(test-case "§3.8 lambda creates a closure"
  (check-true (closure? (ev '(lambda (x) x)))))

;; Closure captures lexical environment
(test-case "§3.8 closure captures defining environment"
  (check-equal? (ev '(let ((y 10))
                       ((lambda (x) (+ x y)) 5)))
                15))

;; --- §3.9 Application ---
;; Apply closure: extend captured env with parameter bindings
(test-case "§3.9 closure application binds parameters"
  (check-equal? (ev '((lambda (a b) (+ a b)) 3 4)) 7))

;; Apply builtin
(test-case "§3.9 builtin application"
  (check-equal? (ev '(+ 1 2 3)) 6))

;; Arity mismatch
(test-case "§3.9 arity mismatch signals error"
  (check-exn #rx"arity mismatch"
             (lambda () (ev '((lambda (x) x) 1 2)))))

;; Not a procedure
(test-case "§3.9 applying non-procedure signals error"
  (check-exn #rx"not a procedure"
             (lambda () (ev '(42 1)))))

;; --- §3.10 Let ---
;; Parallel binding: init exprs evaluated in outer env
(test-case "§3.10 let binds in parallel (outer env for init)"
  (check-equal? (ev '(let ((x 1) (y 2)) (+ x y))) 3))

;; --- §3.10 Let* ---
;; Sequential binding: each init sees previous bindings
(test-case "§3.10 let* binds sequentially"
  (check-equal? (ev '(let* ((x 1) (y (+ x 1))) (+ x y))) 3))

;; --- §3.10 Letrec ---
;; Recursive binding: mutual recursion
(test-case "§3.10 letrec supports recursive definitions"
  (check-equal? (ev '(letrec ((fact (lambda (n)
                                      (if (= n 0) 1 (* n (fact (- n 1)))))))
                       (fact 5)))
                120))

;; --- §3.11 And (short-circuit) ---
;; (and) => #t
(test-case "§3.11 empty and returns #t"
  (check-equal? (ev '(and)) #t))

;; (and e) => value of e
(test-case "§3.11 and with single expression returns its value"
  (check-equal? (ev '(and 42)) 42))

;; Short-circuit: stops at first #f
(test-case "§3.11 and short-circuits on #f"
  (check-equal? (ev '(and 1 #f 3)) #f))

;; All truthy: returns last
(test-case "§3.11 and with all truthy returns last value"
  (check-equal? (ev '(and 1 2 3)) 3))

;; --- §3.11 Or (short-circuit) ---
;; (or) => #f
(test-case "§3.11 empty or returns #f"
  (check-equal? (ev '(or)) #f))

;; (or e) => value of e
(test-case "§3.11 or with single expression returns its value"
  (check-equal? (ev '(or 42)) 42))

;; Short-circuit: returns first truthy
(test-case "§3.11 or returns first truthy value"
  (check-equal? (ev '(or #f #f 5 6)) 5))

;; All false: returns #f
(test-case "§3.11 or with all #f returns #f"
  (check-equal? (ev '(or #f #f #f)) #f))

;; --- §6 Properties ---

;; Property 1: Lexical scope — closure uses defining env, not calling env
(test-case "§6.1 lexical scope: closure resolves free vars in defining env"
  (check-equal? (ev '(let ((x 10))
                       (let ((f (lambda () x))
                             (x 20))
                         (f))))
                10))

;; Property 2: Left-to-right evaluation order
;; push appends to a list; after (+ (push 1) (push 2) (push 3))
;; the accumulator should reflect evaluation order 1, then 2, then 3
(test-case "§6.2 left-to-right evaluation of arguments"
  (let ([env (default-env)])
    (straw-eval '(define acc (quote ())) env)
    (straw-eval '(define (push v) (set! acc (cons v acc)) v) env)
    (straw-eval '(+ (push 1) (push 2) (push 3)) env)
    ;; acc is built by consing, so left-to-right gives (3 2 1)
    (define result (straw-eval 'acc env))
    ;; result is mcons-based list: extract values
    (check-equal? (straw-eval '(car acc) env) 3)
    (check-equal? (straw-eval '(car (cdr acc)) env) 2)
    (check-equal? (straw-eval '(car (cdr (cdr acc))) env) 1)))

;; Property 4: Mutation via shared locations — closures sharing a binding see set!
(test-case "§6.4 mutation visible through shared binding"
  (check-equal? (ev '(let ((x 0))
                       (let ((get (lambda () x))
                             (inc (lambda () (set! x (+ x 1)))))
                         (inc)
                         (inc)
                         (get))))
                2))

;; Property 5: Only #f is false
(test-case "§6.5 only #f is false — empty list is truthy"
  (check-equal? (ev '(if (quote ()) "truthy" "falsy")) "truthy"))

(test-case "§6.5 only #f is false — empty string is truthy"
  (check-equal? (ev '(if "" "truthy" "falsy")) "truthy"))
