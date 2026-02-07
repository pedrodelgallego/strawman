#lang racket
(require "../src/parser.rkt"
         "../src/eval.rkt"
         "../src/env.rkt"
         "../src/builtins.rkt"
         "../src/pretreat.rkt"
         "../src/fast-eval.rkt")
(provide run-benchmarks)

;; Benchmark programs written in Strawman Lisp

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
     (map square (range 1000 (quote ())))))

;; time-eval : (-> value) -> (values value real-milliseconds)
(define (time-eval thunk)
  (define start (current-inexact-milliseconds))
  (define result (thunk))
  (define end (current-inexact-milliseconds))
  (values result (- end start)))

;; run-one : string × s-expr -> (list string real real)
;; Run a benchmark under both evaluators and return (name naive-ms fast-ms).
(define (run-one name prog)
  (define treated (pretreat prog))
  (define-values (_naive naive-ms)
    (time-eval (lambda () (straw-eval prog (default-env)))))
  (define-values (_fast fast-ms)
    (time-eval (lambda () (fast-eval treated (default-env)))))
  (list name naive-ms fast-ms))

;; run-benchmarks : -> (listof (list string real real))
;; Run all benchmarks and return results.
(define (run-benchmarks)
  (list
   (run-one "factorial(20)" factorial-prog)
   (run-one "fibonacci(20)" fibonacci-prog)
   (run-one "ackermann(3,7)" ackermann-prog)
   (run-one "map(square,1000)" map-prog)))

;; When run as a script, print the comparison table.
(module+ main
  (define results (run-benchmarks))
  (printf "~a~n" (make-string 60 #\─))
  (printf "~a ~a ~a ~a~n"
          (~a "Benchmark" #:min-width 20)
          (~a "Naive (ms)" #:min-width 12)
          (~a "Fast (ms)" #:min-width 12)
          (~a "Speedup" #:min-width 8))
  (printf "~a~n" (make-string 60 #\─))
  (for ([r (in-list results)])
    (define name (first r))
    (define naive-ms (second r))
    (define fast-ms (third r))
    (define speedup (if (> fast-ms 0) (/ naive-ms fast-ms) +inf.0))
    (printf "~a ~a ~a ~a~n"
            (~a name #:min-width 20)
            (~a (~r naive-ms #:precision '(= 2)) #:min-width 12)
            (~a (~r fast-ms #:precision '(= 2)) #:min-width 12)
            (~a (~r speedup #:precision '(= 2)) #:min-width 8)
            ))
  (printf "~a~n" (make-string 60 #\─)))
