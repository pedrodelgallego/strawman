#lang racket
(require rackunit
         "../src/eval.rkt"
         "../src/builtins.rkt"
         "../src/parser.rkt")

(define (run code)
  (define exprs (read-all-from-string code))
  (define env (default-env))
  (define result (void))
  (for ([e exprs])
    (set! result (straw-eval e env)))
  result)

;; E9.1 — define-macro: simple macro, swap macro, expansion

(test-case "simple macro: my-if rewrites to if"
  (check-equal?
   (run "(begin (define-macro (my-if c t f) (list 'if c t f)) (my-if #t 1 2))")
   1))

(test-case "simple macro: my-if false branch"
  (check-equal?
   (run "(begin (define-macro (my-if c t f) (list 'if c t f)) (my-if #f 1 2))")
   2))

(test-case "swap macro swaps values"
  (check-equal?
   (run "(begin
           (define-macro (swap! a b)
             (list 'let (list (list 'tmp a))
                   (list 'set! a b)
                   (list 'set! b 'tmp)))
           (define x 1)
           (define y 2)
           (swap! x y)
           (list x y))")
   (mcons 2 (mcons 1 '()))))

(test-case "macroexpand returns expanded form"
  (check-equal?
   (run "(begin (define-macro (my-if c t f) (list 'if c t f)) (macroexpand '(my-if #t 1 2)))")
   '(if #t 1 2)))

(test-case "macroexpand on non-macro returns form unchanged"
  (check-equal?
   (run "(macroexpand '(+ 1 2))")
   '(+ 1 2)))

;; E9.3 — Quasiquote: simple unquote, splicing, nested, no unquote

(test-case "quasiquote: simple unquote"
  (check-equal?
   (run "(let ((x 1)) `(a ,x c))")
   '(a 1 c)))

(test-case "quasiquote: splicing"
  (check-equal?
   (run "(let ((xs '(1 2))) `(a ,@xs c))")
   '(a 1 2 c)))

(test-case "quasiquote: nested quasiquote"
  (check-equal?
   (run "`(a `(b ,(+ 1 2)))")
   '(a (quasiquote (b (unquote (+ 1 2)))))))

(test-case "quasiquote: no unquote"
  (check-equal?
   (run "`(a b c)")
   '(a b c)))

;; E9.4 — Standard macros: cond, when, unless

(test-case "cond first clause matches"
  (check-equal? (run "(cond (#t 1) (#t 2))") 1))

(test-case "cond second clause matches"
  (check-equal? (run "(cond (#f 1) (#t 2))") 2))

(test-case "cond else clause"
  (check-equal? (run "(cond (#f 1) (else 3))") 3))

(test-case "cond no clause matches returns void"
  (check-equal? (run "(cond (#f 1) (#f 2))") (void)))

(test-case "when true evaluates body"
  (check-equal? (run "(when #t 42)") 42))

(test-case "when false returns void"
  (check-equal? (run "(when #f 42)") (void)))

(test-case "unless true returns void"
  (check-equal? (run "(unless #t 42)") (void)))

(test-case "unless false evaluates body"
  (check-equal? (run "(unless #f 42)") 42))

;; Acceptance criteria
(test-case "cond with expressions in test position"
  (check-equal? (run "(cond ((> 3 2) \"yes\") (else \"no\"))") "yes"))
