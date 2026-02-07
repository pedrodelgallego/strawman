#lang racket
(require rackunit
         "../src/eval.rkt"
         "../src/env.rkt"
         "../src/builtins.rkt")

(test-case "Integer evaluates to itself"
  (define e (make-env))
  (check-equal? (straw-eval 42 e) 42))

(test-case "Float evaluates to itself"
  (define e (make-env))
  (check-equal? (straw-eval 3.14 e) 3.14))

(test-case "Negative number evaluates to itself"
  (define e (make-env))
  (check-equal? (straw-eval -7 e) -7))

(test-case "String evaluates to itself"
  (define e (make-env))
  (check-equal? (straw-eval "hello" e) "hello"))

(test-case "Boolean true evaluates to itself"
  (define e (make-env))
  (check-equal? (straw-eval #t e) #t))

(test-case "Boolean false evaluates to itself"
  (define e (make-env))
  (check-equal? (straw-eval #f e) #f))

(test-case "Bound symbol evaluates to its value"
  (define e (make-env))
  (env-set! e 'x 5)
  (check-equal? (straw-eval 'x e) 5))

(test-case "Unbound symbol raises error"
  (define e (make-env))
  (check-exn #rx"unbound variable: y"
             (lambda () (straw-eval 'y e))))

(test-case "Quote symbol returns symbol unevaluated"
  (define e (make-env))
  (check-equal? (straw-eval '(quote foo) e) 'foo))

(test-case "Quote number returns number"
  (define e (make-env))
  (check-equal? (straw-eval '(quote 42) e) 42))

(test-case "Quote list returns list unevaluated"
  (define e (make-env))
  (check-equal? (straw-eval '(quote (1 2 3)) e) '(1 2 3)))

(test-case "Quote nested list returns nested structure"
  (define e (make-env))
  (check-equal? (straw-eval '(quote (a (b c))) e) '(a (b c))))

(test-case "Quote empty list returns empty list"
  (define e (make-env))
  (check-equal? (straw-eval '(quote ()) e) '()))

(test-case "Quote with no args raises arity error"
  (define e (make-env))
  (check-exn #rx"quote expects exactly one argument"
             (lambda () (straw-eval '(quote) e))))

(test-case "Quote with too many args raises arity error"
  (define e (make-env))
  (check-exn #rx"quote expects exactly one argument"
             (lambda () (straw-eval '(quote a b) e))))

(test-case "If true branch is evaluated when test is true"
  (define e (make-env))
  (check-equal? (straw-eval '(if #t 1 2) e) 1))

(test-case "If false branch is evaluated when test is false"
  (define e (make-env))
  (check-equal? (straw-eval '(if #f 1 2) e) 2))

(test-case "Zero is truthy in if"
  (define e (make-env))
  (check-equal? (straw-eval '(if 0 "yes" "no") e) "yes"))

(test-case "Empty list is truthy in if"
  (define e (make-env))
  (check-equal? (straw-eval '(if (quote ()) "yes" "no") e) "yes"))

(test-case "If with no alternative returns consequent when true"
  (define e (make-env))
  (check-equal? (straw-eval '(if #t 42) e) 42))

(test-case "If with no alternative returns void when false"
  (define e (make-env))
  (check-equal? (straw-eval '(if #f 42) e) (void)))

(test-case "Non-taken branch is not evaluated"
  (define e (make-env))
  ;; The alternative is a form that would error if evaluated (unknown form).
  ;; If the non-taken branch were evaluated, this would raise an error.
  (check-equal? (straw-eval '(if #t 1 (this-would-error "boom")) e) 1)
  ;; Likewise when test is false, the consequent should not be evaluated.
  (check-equal? (straw-eval '(if #f (this-would-error "boom") 2) e) 2))

(test-case "If with no args raises arity error"
  (define e (make-env))
  (check-exn #rx"if expects 2 or 3 arguments"
             (lambda () (straw-eval '(if) e))))

(test-case "Begin with single expression returns that expression"
  (define e (make-env))
  (check-equal? (straw-eval '(begin 42) e) 42))

(test-case "Begin with two expressions returns the last"
  (define e (make-env))
  (check-equal? (straw-eval '(begin 1 2) e) 2))

(test-case "Begin with three expressions returns the last"
  (define e (make-env))
  (check-equal? (straw-eval '(begin 1 2 3) e) 3))

(test-case "Empty begin returns void"
  (define e (make-env))
  (check-equal? (straw-eval '(begin) e) (void)))

(test-case "Simple define binds a variable"
  (define e (make-env))
  (check-equal? (straw-eval '(begin (define x 42) x) e) 42))

(test-case "Define overwrites previous binding in same scope"
  (define e (make-env))
  (straw-eval '(define x 1) e)
  (check-equal? (straw-eval 'x e) 1)
  (straw-eval '(define x 2) e)
  (check-equal? (straw-eval 'x e) 2))

(test-case "Define evaluates the value expression"
  (define e (make-env))
  ;; define should evaluate a compound expression, not just literals
  (check-equal? (straw-eval '(begin (define x (if #t 3 0)) x) e) 3)
  ;; define with begin as value expression
  (check-equal? (straw-eval '(begin (define y (begin 1 2 42)) y) e) 42))

(test-case "Define returns void"
  (define e (make-env))
  (check-equal? (straw-eval '(define x 1) e) (void))
  ;; Confirm the binding actually happened despite void return
  (check-equal? (straw-eval 'x e) 1))

(test-case "Nested begin returns the last value of the outer begin"
  (define e (make-env))
  (check-equal? (straw-eval '(begin (begin 1 2) 3) e) 3))

(test-case "Set! updates an existing binding"
  (define e (make-env))
  (check-equal? (straw-eval '(begin (define x 1) (set! x 99) x) e) 99))

(test-case "Set! updates a parent binding from within a lambda"
  (define e (make-env))
  (check-equal? (straw-eval '(begin (define x 1) ((lambda () (set! x 5))) x) e) 5))

(test-case "Set! on unbound variable raises error"
  (define e (make-env))
  (check-exn #rx"cannot set! unbound variable: z"
             (lambda () (straw-eval '(set! z 1) e))))

(test-case "Lambda identity function returns its argument"
  (define e (make-env))
  (check-equal? (straw-eval '((lambda (x) x) 42) e) 42))

(test-case "Lambda with multiple parameters"
  (define e (make-env))
  (env-set! e '+ +)
  (check-equal? (straw-eval '((lambda (x y) (+ x y)) 3 4) e) 7))

(test-case "Closure captures enclosing environment"
  (define e (make-env))
  (env-set! e '+ +)
  (check-equal? (straw-eval '(begin (define a 10)
                                    (define f (lambda (x) (+ x a)))
                                    (f 5))
                            e)
                15))

(test-case "Closure over closure (make-adder)"
  (define e (make-env))
  (env-set! e '+ +)
  (check-equal? (straw-eval '(begin (define make-adder (lambda (n) (lambda (x) (+ n x))))
                                    ((make-adder 3) 7))
                            e)
                10))

(test-case "Lambda body supports implicit begin with define"
  (define e (make-env))
  (env-set! e '+ +)
  (check-equal? (straw-eval '((lambda (x) (define y 1) (+ x y)) 5) e)
                6))

(test-case "Lambda with no parameters"
  (define e (make-env))
  (check-equal? (straw-eval '((lambda () 42)) e) 42))

(test-case "Lambda wrong arity (too many args) raises error"
  (define e (make-env))
  (check-exn #rx"arity mismatch: expected 1, got 2"
             (lambda () (straw-eval '((lambda (x) x) 1 2) e))))

(test-case "Lambda wrong arity (too few args) raises error"
  (define e (make-env))
  (check-exn #rx"arity mismatch: expected 1, got 0"
             (lambda () (straw-eval '((lambda (x) x)) e))))

(test-case "Lambda bad param list raises error"
  (define e (make-env))
  (check-exn #rx"expected parameter list"
             (lambda () (straw-eval '(lambda 42 x) e))))

;; E1.10 — Function application: Builtin call
(test-case "Builtin call: (+ 1 2) returns 3"
  (define e (make-env))
  (env-set! e '+ +)
  (check-equal? (straw-eval '(+ 1 2) e) 3))

;; E1.10 — Function application: Closure call
(test-case "Closure call: ((lambda (x) (* x x)) 5) returns 25"
  (define e (make-env))
  (env-set! e '* *)
  (check-equal? (straw-eval '((lambda (x) (* x x)) 5) e) 25))

;; E1.10 — Function application: Higher-order function
(test-case "Higher-order: ((lambda (f x) (f x)) (lambda (n) (* n 2)) 5) returns 10"
  (define e (make-env))
  (env-set! e '* *)
  (check-equal? (straw-eval '((lambda (f x) (f x)) (lambda (n) (* n 2)) 5) e) 10))

;; E1.10 — Function application: Nested call
(test-case "Nested call: (+ (* 2 3) (- 10 4)) returns 12"
  (define e (make-env))
  (env-set! e '+ +)
  (env-set! e '* *)
  (env-set! e '- -)
  (check-equal? (straw-eval '(+ (* 2 3) (- 10 4)) e) 12))

;; E1.10 — Function application: Non-procedure in operator → error
(test-case "Non-procedure number in operator raises error"
  (define e (make-env))
  (check-exn #rx"not a procedure: 42"
             (lambda () (straw-eval '(42) e))))

(test-case "Non-procedure string in operator raises error"
  (define e (make-env))
  (check-exn #rx"not a procedure: hello"
             (lambda () (straw-eval '("hello" 1) e))))

;; E2.1 — let: Simple binding
(test-case "let with single binding returns body value"
  (define e (make-env))
  (check-equal? (straw-eval '(let ((x 1)) x) e) 1))

;; E2.1 — let: Two bindings
(test-case "let with two bindings evaluates body with both bound"
  (define e (make-env))
  (env-set! e '+ +)
  (check-equal? (straw-eval '(let ((x 1) (y 2)) (+ x y)) e) 3))

;; E2.1 — let: Shadowing / outer unchanged
(test-case "let shadowing does not affect outer environment"
  (define e (make-env))
  ;; Inner let shadows x to 20, but outer x remains 10
  (check-equal? (straw-eval '(begin (define x 10) (let ((x 20)) x) x) e) 10)
  ;; Also verify the let body itself sees the shadowed value
  (define e2 (make-env))
  (check-equal? (straw-eval '(begin (define x 10) (let ((x 20)) x)) e2) 20))

;; E2.1 — let: Parallel semantics (init exprs evaluated in outer env)
(test-case "let parallel semantics: init exprs cannot see other let bindings"
  (define e (make-env))
  (check-exn #rx"unbound variable: x"
             (lambda () (straw-eval '(let ((x 1) (y x)) y) e))))

;; E2.1 — let: Body implicit begin
(test-case "let body supports implicit begin with define"
  (define e (make-env))
  (env-set! e '+ +)
  (check-equal? (straw-eval '(let ((x 1)) (define y 2) (+ x y)) e) 3))

;; E2.1 — let: Nested let
(test-case "nested let accesses outer let bindings"
  (define e (make-env))
  (env-set! e '+ +)
  (check-equal? (straw-eval '(let ((x 1)) (let ((y 2)) (+ x y))) e) 3))

;; E2.1 — let: Empty bindings
(test-case "let with empty bindings evaluates body"
  (define e (make-env))
  (check-equal? (straw-eval '(let () 42) e) 42))

;; E2.1 — let: Malformed binding
(test-case "let with malformed binding list raises error"
  (define e (make-env))
  (check-exn #rx"malformed binding"
             (lambda () (straw-eval '(let (x 1) x) e))))

;; E2.2 — let*: Sequential deps
(test-case "let* sequential: second binding sees first"
  (define e (make-env))
  (env-set! e '+ +)
  (check-equal? (straw-eval '(let* ((x 1) (y (+ x 1))) y) e) 2))

;; E2.2 — let*: Three deps
(test-case "let* three deps: each binding sees all previous"
  (define e (make-env))
  (env-set! e '+ +)
  (check-equal? (straw-eval '(let* ((a 1) (b (+ a 1)) (c (+ b 1))) c) e) 3))

;; E2.2 — let*: Shadow across
(test-case "let* shadow across: same name can be rebound sequentially"
  (define e (make-env))
  (env-set! e '+ +)
  (check-equal? (straw-eval '(let* ((x 1) (x (+ x 1))) x) e) 2))

;; E2.2 — let*: Empty
(test-case "let* with empty bindings evaluates body"
  (define e (make-env))
  (check-equal? (straw-eval '(let* () 42) e) 42))

;; E2.3 — letrec: Non-lambda value
(test-case "letrec with non-lambda value binds simple expression"
  (define e (make-env))
  (check-equal? (straw-eval '(letrec ((x 42)) x) e) 42))

;; E2.3 — letrec: Self-recursive (factorial)
(test-case "letrec self-recursive factorial computes 5!"
  (define e (make-env))
  (env-set! e '<= <=)
  (env-set! e '* *)
  (env-set! e '- -)
  (check-equal? (straw-eval '(letrec ((f (lambda (n) (if (<= n 0) 1 (* n (f (- n 1)))))))
                                (f 5))
                            e)
                120))

;; E2.4 — Define shorthand: Simple
(test-case "define shorthand: simple identity function"
  (define e (make-env))
  (check-equal? (straw-eval '(begin (define (f x) x) (f 42)) e) 42))

;; E2.4 — Define shorthand: Multi-param
(test-case "define shorthand: multiple parameters"
  (define e (make-env))
  (env-set! e '+ +)
  (check-equal? (straw-eval '(begin (define (add a b) (+ a b)) (add 3 4)) e) 7))

;; E2.4 — Define shorthand: With body (implicit begin with internal define)
(test-case "define shorthand: body with internal define"
  (define e (make-env))
  (env-set! e '+ +)
  (check-equal? (straw-eval '(begin (define (g x) (define y 1) (+ x y)) (g 5)) e) 6))

;; E2.4 — Define shorthand: Recursive
(test-case "define shorthand: recursive factorial"
  (define e (make-env))
  (env-set! e '<= <=)
  (env-set! e '* *)
  (env-set! e '- -)
  (check-equal? (straw-eval '(begin (define (fact n) (if (<= n 1) 1 (* n (fact (- n 1))))) (fact 5)) e) 120))

;; E2.4 — Define shorthand: No params
(test-case "define shorthand: no parameters"
  (define e (make-env))
  (check-equal? (straw-eval '(begin (define (f) 42) (f)) e) 42))

;; E2.3 — letrec: Mutual recursion (even?/odd?)
(test-case "letrec mutual recursion: even? and odd? call each other"
  (define e (make-env))
  (env-set! e '= =)
  (env-set! e '- -)
  (check-equal? (straw-eval '(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                                       (odd?  (lambda (n) (if (= n 0) #f (even? (- n 1))))))
                                (even? 4))
                            e)
                #t)
  (check-equal? (straw-eval '(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
                                       (odd?  (lambda (n) (if (= n 0) #f (even? (- n 1))))))
                                (odd? 3))
                            e)
                #t))

;; ─── E2.5 — Integration: Recursion and scoping ────────────────────

;; Factorial 0 / factorial 10
(test-case "E2.5: factorial 0 returns 1"
  (define e (make-env))
  (env-set! e '<= <=)
  (env-set! e '* *)
  (env-set! e '- -)
  (check-equal?
    (straw-eval '(begin
                   (define (fact n) (if (<= n 1) 1 (* n (fact (- n 1)))))
                   (fact 0))
                e)
    1))

(test-case "E2.5: factorial 10 returns 3628800"
  (define e (make-env))
  (env-set! e '<= <=)
  (env-set! e '* *)
  (env-set! e '- -)
  (check-equal?
    (straw-eval '(begin
                   (define (fact n) (if (<= n 1) 1 (* n (fact (- n 1)))))
                   (fact 10))
                e)
    3628800))

;; Fibonacci 10
(test-case "E2.5: fibonacci 10 returns 55"
  (define e (make-env))
  (env-set! e '<= <=)
  (env-set! e '+ +)
  (env-set! e '- -)
  (check-equal?
    (straw-eval '(begin
                   (define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
                   (fib 10))
                e)
    55))

;; E2.5 — Map with lambda
(test-case "E2.5: map with lambda squares a list"
  (define e (default-env))
  (check-equal?
    (straw-eval '(begin
                   (define (map f lst)
                     (if (null? lst)
                         (quote ())
                         (cons (f (car lst)) (map f (cdr lst)))))
                   (map (lambda (x) (* x x)) (quote (1 2 3 4))))
                e)
    (mcons 1 (mcons 4 (mcons 9 (mcons 16 '()))))))

;; E2.5 — Filter with lambda
(test-case "E2.5: filter with lambda selects elements"
  (define e (default-env))
  (check-equal?
    (straw-eval '(begin
                   (define (filter p lst)
                     (if (null? lst)
                         (quote ())
                         (if (p (car lst))
                             (cons (car lst) (filter p (cdr lst)))
                             (filter p (cdr lst)))))
                   (filter (lambda (x) (> x 2)) (quote (1 2 3 4 5))))
                e)
    (mcons 3 (mcons 4 (mcons 5 '())))))

;; E2.5 — Closure scope: lexical vs dynamic
(test-case "E2.5: closure uses lexical scope, not dynamic"
  (define e (default-env))
  ;; f captures x=10 from its definition site.
  ;; g defines a local x=20 and calls f.
  ;; If lexical: f sees x=10 → returns 10.
  ;; If dynamic: f would see x=20 → returns 20.
  (check-equal?
    (straw-eval '(begin
                   (define x 10)
                   (define (f) x)
                   (define (g) (define x 20) (f))
                   (g))
                e)
    10))

;; E2.5 — Accumulator: mutable closure
(test-case "E2.5: accumulator via mutable closure"
  (define e (default-env))
  ;; make-acc returns a closure that mutates captured variable n
  (check-equal?
    (straw-eval '(begin
                   (define (make-acc init)
                     (define n init)
                     (lambda (x) (set! n (+ n x)) n))
                   (define a (make-acc 0))
                   (a 5))
                e)
    5)
  (define e2 (default-env))
  (check-equal?
    (straw-eval '(begin
                   (define (make-acc init)
                     (define n init)
                     (lambda (x) (set! n (+ n x)) n))
                   (define a (make-acc 0))
                   (a 5)
                   (a 3))
                e2)
    8)
  (define e3 (default-env))
  (check-equal?
    (straw-eval '(begin
                   (define (make-acc init)
                     (define n init)
                     (lambda (x) (set! n (+ n x)) n))
                   (define a (make-acc 0))
                   (a 5)
                   (a 3)
                   (a 2))
                e3)
    10))
