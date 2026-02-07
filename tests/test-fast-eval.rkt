#lang racket
(require rackunit
         "../src/parser.rkt"
         "../src/eval.rkt"
         "../src/env.rkt"
         "../src/builtins.rkt"
         "../src/pretreat.rkt"
         "../src/fast-eval.rkt")

;; E6.1 — All existing tests through pretreat + fast-eval → identical results
;; Helper: evaluate an s-expr through both paths and check identical results

(define (eval-both expr env)
  (define expected (straw-eval expr env))
  (define treated (pretreat expr))
  (define actual (fast-eval treated (default-env)))
  (values expected actual))

;; ── Literals ────────────────────────────────────────────────────────

(test-case "fast-eval: integer literal"
  (define treated (pretreat 42))
  (check-equal? (fast-eval treated (default-env)) 42))

(test-case "fast-eval: string literal"
  (define treated (pretreat "hello"))
  (check-equal? (fast-eval treated (default-env)) "hello"))

(test-case "fast-eval: boolean literals"
  (check-equal? (fast-eval (pretreat #t) (default-env)) #t)
  (check-equal? (fast-eval (pretreat #f) (default-env)) #f))

;; ── Variables ───────────────────────────────────────────────────────

(test-case "fast-eval: variable lookup"
  (define e (default-env))
  (env-set! e 'x 5)
  (check-equal? (fast-eval (pretreat 'x) e) 5))

;; ── Quote ───────────────────────────────────────────────────────────

(test-case "fast-eval: quote"
  (define e (default-env))
  (check-equal? (fast-eval (pretreat '(quote foo)) e) 'foo)
  (check-equal? (fast-eval (pretreat '(quote (1 2 3))) e) '(1 2 3)))

;; ── If ──────────────────────────────────────────────────────────────

(test-case "fast-eval: if true branch"
  (define e (default-env))
  (check-equal? (fast-eval (pretreat '(if #t 1 2)) e) 1))

(test-case "fast-eval: if false branch"
  (define e (default-env))
  (check-equal? (fast-eval (pretreat '(if #f 1 2)) e) 2))

;; ── Define & set! ───────────────────────────────────────────────────

(test-case "fast-eval: define and lookup"
  (define e (default-env))
  (check-equal? (fast-eval (pretreat '(begin (define x 42) x)) e) 42))

(test-case "fast-eval: set! updates binding"
  (define e (default-env))
  (check-equal? (fast-eval (pretreat '(begin (define x 1) (set! x 99) x)) e) 99))

;; ── Lambda & application ────────────────────────────────────────────

(test-case "fast-eval: lambda identity"
  (define e (default-env))
  (check-equal? (fast-eval (pretreat '((lambda (x) x) 42)) e) 42))

(test-case "fast-eval: closure captures environment"
  (define e (default-env))
  (check-equal? (fast-eval (pretreat '(begin (define a 10)
                                              (define f (lambda (x) (+ x a)))
                                              (f 5)))
                            e)
                15))

(test-case "fast-eval: higher-order function (make-adder)"
  (define e (default-env))
  (check-equal? (fast-eval (pretreat '(begin (define make-adder (lambda (n) (lambda (x) (+ n x))))
                                              ((make-adder 3) 7)))
                            e)
                10))

;; ── Let, let*, letrec ───────────────────────────────────────────────

(test-case "fast-eval: let"
  (define e (default-env))
  (check-equal? (fast-eval (pretreat '(let ((x 1) (y 2)) (+ x y))) e) 3))

(test-case "fast-eval: let* sequential"
  (define e (default-env))
  (check-equal? (fast-eval (pretreat '(let* ((x 1) (y (+ x 1))) y)) e) 2))

(test-case "fast-eval: letrec self-recursive factorial"
  (define e (default-env))
  (check-equal? (fast-eval (pretreat '(letrec ((f (lambda (n) (if (<= n 0) 1 (* n (f (- n 1)))))))
                                          (f 5)))
                            e)
                120))

;; ── And / Or ────────────────────────────────────────────────────────

(test-case "fast-eval: and short-circuits"
  (define e (default-env))
  (check-equal? (fast-eval (pretreat '(and #t 42)) e) 42)
  (check-equal? (fast-eval (pretreat '(and #f 42)) e) #f))

(test-case "fast-eval: or short-circuits"
  (define e (default-env))
  (check-equal? (fast-eval (pretreat '(or #f 42)) e) 42)
  (check-equal? (fast-eval (pretreat '(or 1 42)) e) 1))

;; ── Begin ───────────────────────────────────────────────────────────

(test-case "fast-eval: begin returns last"
  (define e (default-env))
  (check-equal? (fast-eval (pretreat '(begin 1 2 3)) e) 3))

;; ── Catch / throw ───────────────────────────────────────────────────

(test-case "fast-eval: catch/throw"
  (define e (default-env))
  (check-equal? (fast-eval (pretreat '(catch (quote err) (throw (quote err) 42))) e) 42))

;; ── call/cc ─────────────────────────────────────────────────────────

(test-case "fast-eval: call/cc basic escape"
  (define e (default-env))
  (check-equal? (fast-eval (pretreat '(call/cc (lambda (k) (k 42) 99))) e) 42))

;; ── block / return-from ─────────────────────────────────────────────

(test-case "fast-eval: block/return-from"
  (define e (default-env))
  (check-equal? (fast-eval (pretreat '(block done (return-from done 42) 99)) e) 42))

;; ── unwind-protect ──────────────────────────────────────────────────

(test-case "fast-eval: unwind-protect runs cleanup"
  (define e (default-env))
  (check-equal? (fast-eval (pretreat '(begin
                                         (define result 0)
                                         (unwind-protect
                                           (begin (set! result 1) result)
                                           (set! result (+ result 10)))
                                         result))
                            e)
                11))

;; ── Define shorthand ────────────────────────────────────────────────

(test-case "fast-eval: define shorthand"
  (define e (default-env))
  (check-equal? (fast-eval (pretreat '(begin (define (f x) (+ x 1)) (f 5))) e) 6))

;; ── Builtins via fast-eval ──────────────────────────────────────────

(test-case "fast-eval: arithmetic builtins"
  (define e (default-env))
  (check-equal? (fast-eval (pretreat '(+ 1 2 3)) e) 6)
  (check-equal? (fast-eval (pretreat '(- 10 3)) e) 7)
  (check-equal? (fast-eval (pretreat '(* 2 3 4)) e) 24)
  (check-equal? (fast-eval (pretreat '(/ 10 2)) e) 5))

(test-case "fast-eval: comparison builtins"
  (define e (default-env))
  (check-equal? (fast-eval (pretreat '(< 1 2)) e) #t)
  (check-equal? (fast-eval (pretreat '(> 1 2)) e) #f)
  (check-equal? (fast-eval (pretreat '(= 3 3)) e) #t))

(test-case "fast-eval: list operations"
  (define e (default-env))
  (check-equal? (fast-eval (pretreat '(null? (quote ()))) e) #t)
  (check-equal? (fast-eval (pretreat '(car (cons 1 2))) e) 1)
  (check-equal? (fast-eval (pretreat '(cdr (cons 1 2))) e) 2))

;; ── Recursive programs match straw-eval ─────────────────────────────

(test-case "fast-eval: fibonacci 10 matches straw-eval"
  (define prog '(begin
                   (define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
                   (fib 10)))
  (define e1 (default-env))
  (define e2 (default-env))
  (check-equal? (fast-eval (pretreat prog) e1)
                (straw-eval prog e2)))

(test-case "fast-eval: map with lambda matches straw-eval"
  (define prog '(begin
                   (define (map f lst)
                     (if (null? lst)
                         (quote ())
                         (cons (f (car lst)) (map f (cdr lst)))))
                   (map (lambda (x) (* x x)) (quote (1 2 3 4)))))
  (define e1 (default-env))
  (define e2 (default-env))
  (check-equal? (fast-eval (pretreat prog) e1)
                (straw-eval prog e2)))

;; ── Error cases ─────────────────────────────────────────────────────

(test-case "fast-eval: unbound variable raises error"
  (define e (default-env))
  (check-exn #rx"unbound variable: zzz"
             (lambda () (fast-eval (pretreat 'zzz) e))))

(test-case "fast-eval: not a procedure raises error"
  (define e (default-env))
  (check-exn #rx"not a procedure: 42"
             (lambda () (fast-eval (pretreat '(42)) e))))

;; ── E6.2 — Lexical addressing ────────────────────────────────────────

(test-case "lexical addressing: local var at depth 0, offset 0"
  ;; (lambda (x) x) → body ref for x should be lexical-ref depth=0, offset=0
  (define treated (pretreat '(lambda (x) x)))
  (check-true (treated-lambda? treated))
  (define body-ref (car (treated-lambda-body treated)))
  (check-true (treated-lexical-ref? body-ref))
  (check-equal? (treated-lexical-ref-depth body-ref) 0)
  (check-equal? (treated-lexical-ref-offset body-ref) 0))

(test-case "lexical addressing: free var one level at depth 1, offset 0"
  ;; (lambda (x) (lambda (y) x)) → inner x at depth=1, offset=0
  (define treated (pretreat '(lambda (x) (lambda (y) x))))
  (define inner-lambda (car (treated-lambda-body treated)))
  (check-true (treated-lambda? inner-lambda))
  (define inner-body-ref (car (treated-lambda-body inner-lambda)))
  (check-true (treated-lexical-ref? inner-body-ref))
  (check-equal? (treated-lexical-ref-depth inner-body-ref) 1)
  (check-equal? (treated-lexical-ref-offset inner-body-ref) 0))

(test-case "lexical addressing: multiple params, b at depth 0, offset 1"
  ;; (lambda (a b c) b) → b at depth=0, offset=1
  (define treated (pretreat '(lambda (a b c) b)))
  (define body-ref (car (treated-lambda-body treated)))
  (check-true (treated-lexical-ref? body-ref))
  (check-equal? (treated-lexical-ref-depth body-ref) 0)
  (check-equal? (treated-lexical-ref-offset body-ref) 1))

(test-case "lexical addressing: acceptance criterion — nested lambda x and y"
  ;; (lambda (x) (lambda (y) (+ x y)))
  ;; x in inner body: depth=1, offset=0; y in inner body: depth=0, offset=0
  (define treated (pretreat '(lambda (x) (lambda (y) (+ x y)))))
  (define inner-lambda (car (treated-lambda-body treated)))
  (define app-expr (car (treated-lambda-body inner-lambda)))
  (check-true (treated-app? app-expr))
  (define arg-exprs (treated-app-arg-exprs app-expr))
  (define x-ref (car arg-exprs))
  (define y-ref (cadr arg-exprs))
  (check-true (treated-lexical-ref? x-ref))
  (check-equal? (treated-lexical-ref-depth x-ref) 1)
  (check-equal? (treated-lexical-ref-offset x-ref) 0)
  (check-true (treated-lexical-ref? y-ref))
  (check-equal? (treated-lexical-ref-depth y-ref) 0)
  (check-equal? (treated-lexical-ref-offset y-ref) 0))

(test-case "lexical addressing: fast-eval with lexical refs produces correct results"
  ;; End-to-end: pretreated code with lexical addressing evaluates correctly
  (define e (default-env))
  (check-equal? (fast-eval (pretreat '((lambda (x) x) 42)) e) 42)
  (check-equal? (fast-eval (pretreat '((lambda (a b c) b) 10 20 30)) e) 20)
  (check-equal? (fast-eval (pretreat '((lambda (x) ((lambda (y) (+ x y)) 3)) 5)) e) 8))
