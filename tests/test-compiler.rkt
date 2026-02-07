#lang racket
(require rackunit
         "../src/compiler.rkt")

;; E7.2 — Compiler: S-expression to bytecode
;; Tests that the compiler translates core forms to correct bytecode sequences.

;; ============================================================
;; Test Matrix from spec.md
;; ============================================================

(test-case "compile number literal"
  ;; Number 42 → (CONST 42)
  (check-equal? (compile 42)
                '((CONST 42))))

(test-case "compile string literal"
  (check-equal? (compile "hello")
                '((CONST "hello"))))

(test-case "compile boolean literal"
  (check-equal? (compile #t)
                '((CONST #t)))
  (check-equal? (compile #f)
                '((CONST #f))))

(test-case "compile symbol reference"
  ;; Symbol x → (LOOKUP x)
  (check-equal? (compile 'x)
                '((LOOKUP x))))

(test-case "compile quote"
  (check-equal? (compile '(quote (1 2 3)))
                '((CONST (1 2 3))))
  (check-equal? (compile '(quote hello))
                '((CONST hello))))

(test-case "compile if with three branches"
  ;; (if #t 1 2) →
  ;;   CONST #t
  ;;   JUMP-IF-FALSE L1
  ;;   CONST 1
  ;;   JUMP L2
  ;; L1: CONST 2
  ;; L2:
  (define code (compile '(if #t 1 2)))
  ;; Check structure: CONST #t, JUMP-IF-FALSE, CONST 1, JUMP, CONST 2
  (check-equal? (first code) '(CONST #t))
  (check-equal? (first (second code)) 'JUMP-IF-FALSE)
  (check-equal? (third code) '(CONST 1))
  (check-equal? (first (fourth code)) 'JUMP)
  (check-equal? (fifth code) '(CONST 2))
  ;; JUMP-IF-FALSE should point to the alternative (index 4)
  (check-equal? (second (second code)) 4)
  ;; JUMP should point past the alternative (index 5)
  (check-equal? (second (fourth code)) 5))

(test-case "compile if with two branches (no alternative)"
  (define code (compile '(if #t 1)))
  ;; Should produce: CONST #t, JUMP-IF-FALSE, CONST 1, JUMP, CONST void
  (check-equal? (first code) '(CONST #t))
  (check-equal? (first (second code)) 'JUMP-IF-FALSE)
  (check-equal? (third code) '(CONST 1))
  (check-equal? (first (fourth code)) 'JUMP)
  (check-equal? (fifth code) (list 'CONST (void))))

(test-case "compile application"
  ;; (+ 1 2) → LOOKUP +, CONST 1, CONST 2, CALL 2
  (check-equal? (compile '(+ 1 2))
                '((LOOKUP +) (CONST 1) (CONST 2) (CALL 2))))

(test-case "compile nested application"
  ;; (+ 1 (+ 2 3)) → LOOKUP +, CONST 1, LOOKUP +, CONST 2, CONST 3, CALL 2, CALL 2
  (check-equal? (compile '(+ 1 (+ 2 3)))
                '((LOOKUP +) (CONST 1) (LOOKUP +) (CONST 2) (CONST 3) (CALL 2) (CALL 2))))

;; ============================================================
;; Additional core forms
;; ============================================================

(test-case "compile begin"
  ;; (begin 1 2 3) → CONST 1, POP, CONST 2, POP, CONST 3
  (check-equal? (compile '(begin 1 2 3))
                '((CONST 1) (POP) (CONST 2) (POP) (CONST 3))))

(test-case "compile begin single expression"
  (check-equal? (compile '(begin 42))
                '((CONST 42))))

(test-case "compile define variable"
  ;; (define x 42) → CONST 42, DEFINE x
  (check-equal? (compile '(define x 42))
                '((CONST 42) (DEFINE x))))

(test-case "compile define function shorthand"
  ;; (define (f x) x) desugars to (define f (lambda (x) x))
  (define code (compile '(define (f x) x)))
  ;; Should start with CLOSURE for the lambda, then DEFINE f
  (check-equal? (first (first code)) 'CLOSURE)
  (check-equal? (car (last code)) 'DEFINE)
  (check-equal? (cadr (last code)) 'f))

(test-case "compile set!"
  ;; (set! x 10) → CONST 10, SET x
  (check-equal? (compile '(set! x 10))
                '((CONST 10) (SET x))))

(test-case "compile lambda"
  ;; (lambda (x) x) → CLOSURE L1, JUMP L2, L1: LOOKUP x, RETURN, L2:
  (define code (compile '(lambda (x) x)))
  (check-equal? (first (first code)) 'CLOSURE)
  (check-equal? (first (second code)) 'JUMP)
  ;; The closure body should contain LOOKUP x then RETURN
  (define body-start (second (first code)))
  (check-equal? (list-ref code body-start) '(LOOKUP x))
  (check-equal? (list-ref code (+ body-start 1)) '(RETURN)))

(test-case "compile lambda with multi-expression body"
  ;; (lambda (x) 1 x) → CLOSURE, JUMP, body: CONST 1, POP, LOOKUP x, RETURN
  (define code (compile '(lambda (x) 1 x)))
  (define body-start (second (first code)))
  (check-equal? (list-ref code body-start) '(CONST 1))
  (check-equal? (list-ref code (+ body-start 1)) '(POP))
  (check-equal? (list-ref code (+ body-start 2)) '(LOOKUP x))
  (check-equal? (list-ref code (+ body-start 3)) '(RETURN)))

(test-case "compile and with no arguments"
  ;; (and) → CONST #t
  (check-equal? (compile '(and))
                '((CONST #t))))

(test-case "compile and with arguments"
  ;; (and a b) → LOOKUP a, JUMP-IF-FALSE Lend, LOOKUP b, JUMP Ldone, Lend: CONST #f, Ldone:
  (define code (compile '(and a b)))
  (check-equal? (first code) '(LOOKUP a))
  (check-equal? (first (second code)) 'JUMP-IF-FALSE)
  (check-equal? (third code) '(LOOKUP b))
  (check-equal? (first (fourth code)) 'JUMP)
  ;; After JUMP should be CONST #f
  (define lend (second (second code)))
  (check-equal? (list-ref code lend) '(CONST #f)))

(test-case "compile or with no arguments"
  ;; (or) → CONST #f
  (check-equal? (compile '(or))
                '((CONST #f))))

(test-case "compile or with arguments"
  ;; (or a b) desugars to (let ((t a)) (if t t b))
  ;; So output starts with CLOSURE for the let lambda, then LOOKUP a, then CALL 1
  (define code (compile '(or a b)))
  ;; Should start with CLOSURE (let's lambda)
  (check-equal? (first (first code)) 'CLOSURE)
  ;; Should end with CALL 1
  (define last-instr (last code))
  (check-equal? (first last-instr) 'CALL)
  (check-equal? (second last-instr) 1))

(test-case "compile let"
  ;; (let ((x 1)) x) compiles as: CLOSURE (lambda), init expr, CALL 1
  ;; Lambda (proc) is pushed first, then init values (args)
  (define code (compile '(let ((x 1)) x)))
  ;; Should start with CLOSURE for the lambda
  (check-equal? (first (first code)) 'CLOSURE)
  ;; Should end with a CALL
  (define last-instr (last code))
  (check-equal? (first last-instr) 'CALL)
  (check-equal? (second last-instr) 1))

(test-case "compile letrec"
  ;; (letrec ((x 1)) x) → CONST void, DEFINE x, CONST 1, SET x, LOOKUP x
  (define code (compile '(letrec ((x 1)) x)))
  (check-equal? (first code) (list 'CONST (void)))
  (check-equal? (second code) '(DEFINE x))
  (check-equal? (third code) '(CONST 1))
  (check-equal? (fourth code) '(SET x))
  (check-equal? (fifth code) '(LOOKUP x)))

(test-case "compile zero-argument application"
  ;; (f) → LOOKUP f, CALL 0
  (check-equal? (compile '(f))
                '((LOOKUP f) (CALL 0))))
