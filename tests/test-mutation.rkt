#lang racket
(require rackunit
         "../src/parser.rkt"
         "../src/eval.rkt"
         "../src/builtins.rkt")

;; Helper: parse and evaluate a Strawman expression string
(define (run str)
  (straw-eval (read-from-string str) (default-env)))

;; E4.1 — Box model for variables

(test-case "shared mutation: two closures sharing a top-level variable see mutations"
  ;; inc mutates x, get reads x — they share the same binding
  (check-equal?
   (run "(begin (define x 0) (define inc (lambda () (set! x (+ x 1)))) (define get (lambda () x)) (inc) (inc) (get))")
   2))

(test-case "lambda captures box: closures from same function share local variable"
  ;; make returns a pair of closures sharing local n
  ;; First call to inc returns 1, second returns 2, get returns 2
  (define env (default-env))
  (check-equal? (straw-eval (read-from-string
    "(begin (define (make) (define n 0) (list (lambda () (set! n (+ n 1)) n) (lambda () n))) (define pair (make)) ((car pair)))")
    env) 1)

  ;; Continue in the same environment — call inc again
  (check-equal? (straw-eval (read-from-string "((car pair))") env) 2)

  ;; Call get — should see the mutations from inc
  (check-equal? (straw-eval (read-from-string "((car (cdr pair)))") env) 2))

;; E4.2 — set-car! / set-cdr!

(test-case "set-car! mutates the car of a pair"
  (check-equal?
   (run "(begin (define p (cons 1 2)) (set-car! p 10) (car p))")
   10))

(test-case "set-cdr! mutates the cdr of a pair"
  (check-equal?
   (run "(begin (define p (cons 1 2)) (set-cdr! p 20) (cdr p))")
   20))

(test-case "set-car! on non-pair raises error"
  (check-exn
   #rx"set-car!: expected mutable pair"
   (lambda () (run "(set-car! 42 1)"))))

;; E4.3 — eq? vs equal?: identity vs structural equality

(test-case "eq? same symbol: (eq? 'a 'a) returns #t"
  (check-equal? (run "(eq? 'a 'a)") #t))

(test-case "eq? same number: (eq? 42 42) returns #t"
  (check-equal? (run "(eq? 42 42)") #t))

(test-case "eq? different lists: (eq? (list 1) (list 1)) returns #f"
  (check-equal? (run "(eq? (list 1) (list 1))") #f))

(test-case "eq? same binding: same object is eq? to itself"
  (check-equal?
   (run "(begin (define x (list 1)) (eq? x x))")
   #t))

(test-case "equal? lists: (equal? (list 1 2) (list 1 2)) returns #t"
  (check-equal? (run "(equal? (list 1 2) (list 1 2))") #t))

(test-case "equal? nested: (equal? '(1 (2 3)) '(1 (2 3))) returns #t"
  (check-equal? (run "(equal? '(1 (2 3)) '(1 (2 3)))") #t))

(test-case "equal? different: (equal? '(1 2) '(1 3)) returns #f"
  (check-equal? (run "(equal? '(1 2) '(1 3))") #f))

(test-case "acceptance: eq? is #f but equal? is #t for structurally equal lists"
  (check-equal?
   (run "(begin (define a (list 1 2)) (define b (list 1 2)) (list (eq? a b) (equal? a b)))")
   (run "(list #f #t)")))

;; E4.4 — Vectors: make/ref, set/ref, length, type pred, out of bounds

(test-case "make-vector and vector-ref: default fill"
  (check-equal?
   (run "(begin (define v (make-vector 3 0)) (vector-ref v 0))")
   0))

(test-case "vector-set! and vector-ref"
  (check-equal?
   (run "(begin (define v (make-vector 3 0)) (vector-set! v 1 42) (vector-ref v 1))")
   42))

(test-case "vector-length"
  (check-equal?
   (run "(vector-length (make-vector 5))")
   5))

(test-case "vector? positive: vector is a vector"
  (check-equal?
   (run "(vector? (make-vector 1))")
   #t))

(test-case "vector? negative: list is not a vector"
  (check-equal?
   (run "(vector? '(1 2))")
   #f))

(test-case "vector-ref out of bounds raises error"
  (check-exn
   #rx"vector-ref: index out of range"
   (lambda () (run "(vector-ref (make-vector 3) 5)"))))

;; Acceptance criteria
(test-case "acceptance: vector-set! and vector-ref round-trip"
  (check-equal?
   (run "(begin (define v (make-vector 3 0)) (vector-set! v 2 99) (vector-ref v 2))")
   99))
