#lang racket
(require rackunit
         "../src/compiler.rkt"
         "../src/vm.rkt"
         "../src/builtins.rkt")

;; E7.3 — VM execute: all existing tests via compile + VM → identical results
;; Helper: compile an expression, run it on the VM with the default environment,
;; and return the result.
(define (run expr)
  (define code (compile expr))
  (vm-execute code (default-env)))

;; ============================================================
;; Literals
;; ============================================================

(test-case "vm: number literal"
  (check-equal? (run 42) 42))

(test-case "vm: string literal"
  (check-equal? (run "hello") "hello"))

(test-case "vm: boolean literals"
  (check-equal? (run #t) #t)
  (check-equal? (run #f) #f))

;; ============================================================
;; Quote
;; ============================================================

(test-case "vm: quoted symbol"
  (check-equal? (run '(quote hello)) 'hello))

(test-case "vm: quoted list"
  (check-equal? (run '(quote (1 2 3))) '(1 2 3)))

;; ============================================================
;; Arithmetic (builtins)
;; ============================================================

(test-case "vm: addition"
  (check-equal? (run '(+ 1 2)) 3))

(test-case "vm: nested arithmetic"
  (check-equal? (run '(+ 1 (+ 2 3))) 6))

(test-case "vm: subtraction"
  (check-equal? (run '(- 10 3)) 7))

(test-case "vm: multiplication"
  (check-equal? (run '(* 4 5)) 20))

(test-case "vm: division"
  (check-equal? (run '(/ 10 2)) 5))

;; ============================================================
;; Comparisons
;; ============================================================

(test-case "vm: less-than"
  (check-equal? (run '(< 1 2)) #t)
  (check-equal? (run '(< 2 1)) #f))

(test-case "vm: equal?"
  (check-equal? (run '(equal? 1 1)) #t)
  (check-equal? (run '(equal? 1 2)) #f))

;; ============================================================
;; If
;; ============================================================

(test-case "vm: if true branch"
  (check-equal? (run '(if #t 1 2)) 1))

(test-case "vm: if false branch"
  (check-equal? (run '(if #f 1 2)) 2))

(test-case "vm: if without alternative (true)"
  (check-equal? (run '(if #t 42)) 42))

(test-case "vm: if without alternative (false)"
  (check-equal? (run '(if #f 42)) (void)))

(test-case "vm: nested if"
  (check-equal? (run '(if (< 1 2) (+ 10 20) 0)) 30))

;; ============================================================
;; Begin
;; ============================================================

(test-case "vm: begin returns last value"
  (check-equal? (run '(begin 1 2 3)) 3))

(test-case "vm: begin single expression"
  (check-equal? (run '(begin 42)) 42))

;; ============================================================
;; Define + variable reference
;; ============================================================

(test-case "vm: define and lookup"
  (check-equal? (run '(begin (define x 42) x)) 42))

(test-case "vm: define function shorthand and call"
  (check-equal? (run '(begin (define (double n) (+ n n)) (double 5))) 10))

;; ============================================================
;; Set!
;; ============================================================

(test-case "vm: set! updates variable"
  (check-equal? (run '(begin (define x 1) (set! x 2) x)) 2))

;; ============================================================
;; Lambda
;; ============================================================

(test-case "vm: lambda and application"
  (check-equal? (run '((lambda (x) x) 42)) 42))

(test-case "vm: lambda with multiple params"
  (check-equal? (run '((lambda (x y) (+ x y)) 3 4)) 7))

(test-case "vm: lambda with multi-expression body"
  (check-equal? (run '((lambda (x) 1 (+ x 10)) 5)) 15))

(test-case "vm: closure captures environment"
  (check-equal? (run '(begin
                         (define (make-adder n)
                           (lambda (x) (+ n x)))
                         ((make-adder 10) 5)))
                15))

(test-case "vm: higher-order function"
  (check-equal? (run '(begin
                         (define (apply-fn f x) (f x))
                         (apply-fn (lambda (n) (+ n 1)) 41)))
                42))

;; ============================================================
;; And / Or
;; ============================================================

(test-case "vm: and with no args"
  (check-equal? (run '(and)) #t))

(test-case "vm: and short-circuits on false"
  (check-equal? (run '(and #f 42)) #f))

(test-case "vm: and returns last truthy"
  (check-equal? (run '(and 1 2 3)) 3))

(test-case "vm: or with no args"
  (check-equal? (run '(or)) #f))

(test-case "vm: or short-circuits on true"
  (check-equal? (run '(or 1 2)) 1))

(test-case "vm: or returns last when all false"
  (check-equal? (run '(or #f #f 3)) 3))

;; ============================================================
;; Let / Let* / Letrec
;; ============================================================

(test-case "vm: let binding"
  (check-equal? (run '(let ((x 1) (y 2)) (+ x y))) 3))

(test-case "vm: let* sequential binding"
  (check-equal? (run '(let* ((x 1) (y (+ x 1))) (+ x y))) 3))

(test-case "vm: letrec with mutual reference"
  (check-equal? (run '(letrec ((fact (lambda (n)
                                        (if (= n 0) 1 (* n (fact (- n 1)))))))
                         (fact 5)))
                120))

;; ============================================================
;; List operations
;; ============================================================

(test-case "vm: cons, car, cdr"
  (check-equal? (run '(car (cons 1 2))) 1)
  (check-equal? (run '(cdr (cons 1 2))) 2))

(test-case "vm: null? on empty list"
  (check-equal? (run '(null? (quote ()))) #t))

(test-case "vm: pair?"
  (check-equal? (run '(pair? (cons 1 2))) #t))

;; ============================================================
;; Type predicates
;; ============================================================

(test-case "vm: number?"
  (check-equal? (run '(number? 42)) #t)
  (check-equal? (run '(number? "hi")) #f))

(test-case "vm: string?"
  (check-equal? (run '(string? "hi")) #t)
  (check-equal? (run '(string? 42)) #f))

(test-case "vm: boolean?"
  (check-equal? (run '(boolean? #t)) #t)
  (check-equal? (run '(boolean? 0)) #f))

;; ============================================================
;; Zero-argument function
;; ============================================================

(test-case "vm: zero-argument lambda"
  (check-equal? (run '((lambda () 42))) 42))

;; ============================================================
;; Error handling
;; ============================================================

(test-case "vm: stack underflow error"
  (check-exn exn:fail?
    (lambda () (vm-execute '((POP)) (default-env)))))

(test-case "vm: unbound variable error"
  (check-exn #rx"unbound variable"
    (lambda () (run 'undefined-var))))

(test-case "vm: not a procedure error"
  (check-exn #rx"not a procedure"
    (lambda () (run '(42 1 2)))))

;; ============================================================
;; Recursion via define
;; ============================================================

(test-case "vm: recursive function"
  (check-equal? (run '(begin
                         (define (fib n)
                           (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
                         (fib 10)))
                55))

;; ============================================================
;; Vectors
;; ============================================================

(test-case "vm: vector operations"
  (check-equal? (run '(begin
                         (define v (make-vector 3 0))
                         (vector-set! v 1 42)
                         (vector-ref v 1)))
                42))

;; ============================================================
;; Nested closures and scope
;; ============================================================

(test-case "vm: nested closures"
  (check-equal? (run '(begin
                         (define (make-counter)
                           (define count 0)
                           (lambda ()
                             (set! count (+ count 1))
                             count))
                         (define c (make-counter))
                         (c)
                         (c)
                         (c)))
                3))
