#lang racket
(require rackunit
         "../src/parser.rkt"
         "../src/eval.rkt"
         "../src/env.rkt"
         "../src/builtins.rkt"
         "../src/pretreat.rkt"
         "../src/fast-eval.rkt")

;; E6.3 — Benchmark harness: factorial, fibonacci, ackermann, map
;; Tests that benchmark programs produce correct results under both evaluators,
;; and that the harness module exists and can run benchmarks.

;; ── Benchmark programs produce correct results ──────────────────────

(define factorial-prog
  '(begin
     (define (fact n) (if (<= n 0) 1 (* n (fact (- n 1)))))
     (fact 20)))

(define fibonacci-prog
  '(begin
     (define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
     (fib 20)))

(define ackermann-prog
  '(begin
     (define (ack m n)
       (if (= m 0)
           (+ n 1)
           (if (= n 0)
               (ack (- m 1) 1)
               (ack (- m 1) (ack m (- n 1))))))
     (ack 3 7)))

(define map-prog
  '(begin
     (define (map f lst)
       (if (null? lst)
           (quote ())
           (cons (f (car lst)) (map f (cdr lst)))))
     (define (range n acc)
       (if (<= n 0)
           acc
           (range (- n 1) (cons n acc))))
     (define (square x) (* x x))
     (car (map square (range 100 (quote ()))))))

;; Both evaluators produce the same correct results

(test-case "benchmark: factorial(20) — naive eval"
  (check-equal? (straw-eval factorial-prog (default-env)) 2432902008176640000))

(test-case "benchmark: factorial(20) — fast eval"
  (check-equal? (fast-eval (pretreat factorial-prog) (default-env)) 2432902008176640000))

(test-case "benchmark: fibonacci(20) — both evaluators agree"
  (define e1 (default-env))
  (define e2 (default-env))
  (define naive-result (straw-eval fibonacci-prog e1))
  (define fast-result (fast-eval (pretreat fibonacci-prog) e2))
  (check-equal? naive-result 6765)
  (check-equal? fast-result 6765))

(test-case "benchmark: ackermann(3,7) — both evaluators agree"
  (define e1 (default-env))
  (define e2 (default-env))
  (define naive-result (straw-eval ackermann-prog e1))
  (define fast-result (fast-eval (pretreat ackermann-prog) e2))
  (check-equal? naive-result 1021)
  (check-equal? fast-result 1021))

(test-case "benchmark: map over list — both evaluators agree"
  (define e1 (default-env))
  (define e2 (default-env))
  (define naive-result (straw-eval map-prog e1))
  (define fast-result (fast-eval (pretreat map-prog) e2))
  (check-equal? naive-result fast-result)
  ;; (square 1) = 1
  (check-equal? naive-result 1))

;; ── Harness module exists and runs ───────────────────────────────────

(test-case "benchmark: run-benchmarks function exists and returns results"
  (define bench-mod (dynamic-require "../bench/run.rkt" 'run-benchmarks))
  (check-true (procedure? bench-mod))
  (define results (bench-mod))
  ;; results should be a list of benchmark entries
  (check-true (list? results))
  (check-true (>= (length results) 4))
  ;; Each entry should have name, naive-ms, fast-ms
  (for ([entry (in-list results)])
    (check-true (list? entry))
    (check-true (>= (length entry) 3))
    (check-true (string? (first entry)))        ; name
    (check-true (number? (second entry)))       ; naive-ms
    (check-true (number? (third entry)))))      ; fast-ms
