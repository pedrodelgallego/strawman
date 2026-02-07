#lang racket
(require rackunit
         "../src/eval.rkt"
         "../src/builtins.rkt")

(test-case "Add zero args: (+) returns 0"
  (define e (default-env))
  (check-equal? (straw-eval '(+) e) 0))

(test-case "Add one arg: (+ 5) returns 5"
  (define e (default-env))
  (check-equal? (straw-eval '(+ 5) e) 5))

(test-case "Add many args: (+ 1 2 3 4) returns 10"
  (define e (default-env))
  (check-equal? (straw-eval '(+ 1 2 3 4) e) 10))

(test-case "Add floats: (+ 1.5 2.5) returns 4.0"
  (define e (default-env))
  (check-equal? (straw-eval '(+ 1.5 2.5) e) 4.0))

(test-case "Sub negate: (- 5) returns -5"
  (define e (default-env))
  (check-equal? (straw-eval '(- 5) e) -5))

(test-case "Sub two: (- 10 3) returns 7"
  (define e (default-env))
  (check-equal? (straw-eval '(- 10 3) e) 7))

(test-case "Sub many: (- 10 3 2) returns 5"
  (define e (default-env))
  (check-equal? (straw-eval '(- 10 3 2) e) 5))

(test-case "Mul zero args: (*) returns 1"
  (define e (default-env))
  (check-equal? (straw-eval '(*) e) 1))

(test-case "Mul: (* 3 4) returns 12"
  (define e (default-env))
  (check-equal? (straw-eval '(* 3 4) e) 12))

(test-case "Div int: (/ 10 2) returns 5"
  (define e (default-env))
  (check-equal? (straw-eval '(/ 10 2) e) 5))

(test-case "Div float: (/ 7 2) returns 3.5"
  (define e (default-env))
  (check-equal? (straw-eval '(/ 7 2) e) 3.5))

(test-case "Div zero: (/ 1 0) raises division by zero error"
  (define e (default-env))
  (check-exn #rx"division by zero"
    (lambda () (straw-eval '(/ 1 0) e))))

(test-case "Mod: (mod 10 3) returns 1"
  (define e (default-env))
  (check-equal? (straw-eval '(mod 10 3) e) 1))

(test-case "Mod zero: (mod 10 0) raises division by zero error"
  (define e (default-env))
  (check-exn #rx"division by zero"
    (lambda () (straw-eval '(mod 10 0) e))))

(test-case "Non-number arg: (+ 1 \"a\") raises expected number error"
  (define e (default-env))
  (check-exn #rx"expected number"
    (lambda () (straw-eval '(+ 1 "a") e))))

;; E1.12 — Comparison operators: <, >, <=, >=

(test-case "Less true: (< 1 2) returns #t"
  (define e (default-env))
  (check-equal? (straw-eval '(< 1 2) e) #t))

(test-case "Less false: (< 2 1) returns #f"
  (define e (default-env))
  (check-equal? (straw-eval '(< 2 1) e) #f))

(test-case "Less equal: (< 2 2) returns #f"
  (define e (default-env))
  (check-equal? (straw-eval '(< 2 2) e) #f))

(test-case "Greater true: (> 3 1) returns #t"
  (define e (default-env))
  (check-equal? (straw-eval '(> 3 1) e) #t))

(test-case "Leq true: (<= 2 2) returns #t"
  (define e (default-env))
  (check-equal? (straw-eval '(<= 2 2) e) #t))

(test-case "Geq false: (>= 1 2) returns #f"
  (define e (default-env))
  (check-equal? (straw-eval '(>= 1 2) e) #f))

;; E1.12 — Numeric equality: =

(test-case "Num eq: (= 5 5) returns #t"
  (define e (default-env))
  (check-equal? (straw-eval '(= 5 5) e) #t))

(test-case "Num neq: (= 5 6) returns #f"
  (define e (default-env))
  (check-equal? (straw-eval '(= 5 6) e) #f))

;; E1.12 — equal?: deep structural equality

(test-case "Equal? atoms: (equal? 42 42) returns #t"
  (define e (default-env))
  (check-equal? (straw-eval '(equal? 42 42) e) #t))

(test-case "Equal? strings: (equal? \"ab\" \"ab\") returns #t"
  (define e (default-env))
  (check-equal? (straw-eval '(equal? "ab" "ab") e) #t))

(test-case "Equal? lists: (equal? '(1 2) '(1 2)) returns #t"
  (define e (default-env))
  (check-equal? (straw-eval '(equal? (quote (1 2)) (quote (1 2))) e) #t))

(test-case "Equal? nested: (equal? (list 1 (list 2 3)) (list 1 (list 2 3))) returns #t"
  (define e (default-env))
  (check-equal? (straw-eval '(equal? (list 1 (list 2 3)) (list 1 (list 2 3))) e) #t))

(test-case "Equal? diff: (equal? '(1 2) '(1 3)) returns #f"
  (define e (default-env))
  (check-equal? (straw-eval '(equal? (quote (1 2)) (quote (1 3))) e) #f))

;; E1.12 — Non-number arg to comparison operator

(test-case "Non-number arg: (< \"a\" \"b\") raises expected number error"
  (define e (default-env))
  (check-exn #rx"expected number"
    (lambda () (straw-eval '(< "a" "b") e))))

;; E1.13 — List operations: cons

(test-case "Cons onto list: (cons 1 '(2 3)) returns (1 2 3)"
  (define e (default-env))
  (check-equal? (straw-eval '(cons 1 (quote (2 3))) e) '(1 2 3)))

(test-case "Cons dotted: (cons 1 2) returns (1 . 2)"
  (define e (default-env))
  (check-equal? (straw-eval '(cons 1 2) e) '(1 . 2)))

;; E1.13 — List operations: car / cdr

(test-case "Car: (car '(a b c)) returns a"
  (define e (default-env))
  (check-equal? (straw-eval '(car (quote (a b c))) e) 'a))

(test-case "Cdr: (cdr '(a b c)) returns (b c)"
  (define e (default-env))
  (check-equal? (straw-eval '(cdr (quote (a b c))) e) '(b c)))

(test-case "Car of cons: (car (cons 1 2)) returns 1"
  (define e (default-env))
  (check-equal? (straw-eval '(car (cons 1 2)) e) 1))

(test-case "Cdr of cons: (cdr (cons 1 2)) returns 2"
  (define e (default-env))
  (check-equal? (straw-eval '(cdr (cons 1 2)) e) 2))

(test-case "Car of empty list raises error"
  (define e (default-env))
  (check-exn #rx"car: expected pair"
    (lambda () (straw-eval '(car (quote ())) e))))

(test-case "Cdr of atom raises error"
  (define e (default-env))
  (check-exn #rx"cdr: expected pair"
    (lambda () (straw-eval '(cdr 42) e))))

;; E1.13 — List operations: list

(test-case "List empty: (list) returns empty list"
  (define e (default-env))
  (check-equal? (straw-eval '(list) e) '()))

(test-case "List many: (list 1 2 3) returns (1 2 3)"
  (define e (default-env))
  (check-equal? (straw-eval '(list 1 2 3) e) '(1 2 3)))

;; E1.13 — List operations: null? / pair?

(test-case "Null? empty: (null? '()) returns #t"
  (define e (default-env))
  (check-equal? (straw-eval '(null? (quote ())) e) #t))

(test-case "Null? non-empty: (null? '(1)) returns #f"
  (define e (default-env))
  (check-equal? (straw-eval '(null? (quote (1))) e) #f))

(test-case "Null? non-list: (null? 42) returns #f"
  (define e (default-env))
  (check-equal? (straw-eval '(null? 42) e) #f))

(test-case "Pair? pair: (pair? '(1 2)) returns #t"
  (define e (default-env))
  (check-equal? (straw-eval '(pair? (quote (1 2))) e) #t))

(test-case "Pair? empty: (pair? '()) returns #f"
  (define e (default-env))
  (check-equal? (straw-eval '(pair? (quote ())) e) #f))

(test-case "Pair? atom: (pair? 42) returns #f"
  (define e (default-env))
  (check-equal? (straw-eval '(pair? 42) e) #f))

;; E1.14 — and (special form, short-circuit)

(test-case "And all true: (and 1 2 3) returns 3"
  (define e (default-env))
  (check-equal? (straw-eval '(and 1 2 3) e) 3))

(test-case "And short-circuit: (and #f (error \"boom\")) returns #f"
  (define e (default-env))
  (check-equal? (straw-eval '(and #f (/ 1 0)) e) #f))

(test-case "And empty: (and) returns #t"
  (define e (default-env))
  (check-equal? (straw-eval '(and) e) #t))

(test-case "And one false: (and 1 #f 3) returns #f"
  (define e (default-env))
  (check-equal? (straw-eval '(and 1 #f 3) e) #f))

;; E1.14 — or (special form, short-circuit)

(test-case "Or first true: (or 1 2) returns 1"
  (define e (default-env))
  (check-equal? (straw-eval '(or 1 2) e) 1))

(test-case "Or all false: (or #f #f) returns #f"
  (define e (default-env))
  (check-equal? (straw-eval '(or #f #f) e) #f))

(test-case "Or short-circuit: (or 1 (/ 1 0)) returns 1"
  (define e (default-env))
  (check-equal? (straw-eval '(or 1 (/ 1 0)) e) 1))

(test-case "Or empty: (or) returns #f"
  (define e (default-env))
  (check-equal? (straw-eval '(or) e) #f))

;; E1.14 — not (builtin)

(test-case "Not true: (not #t) returns #f"
  (define e (default-env))
  (check-equal? (straw-eval '(not #t) e) #f))

(test-case "Not false: (not #f) returns #t"
  (define e (default-env))
  (check-equal? (straw-eval '(not #f) e) #t))

(test-case "Not truthy: (not 42) returns #f"
  (define e (default-env))
  (check-equal? (straw-eval '(not 42) e) #f))

;; E1.15 — Type predicates: positive and negative cases

(test-case "number? positive: (number? 42) returns #t"
  (define e (default-env))
  (check-equal? (straw-eval '(number? 42) e) #t))

(test-case "number? negative: (number? \"x\") returns #f"
  (define e (default-env))
  (check-equal? (straw-eval '(number? "x") e) #f))

(test-case "string? positive: (string? \"hi\") returns #t"
  (define e (default-env))
  (check-equal? (straw-eval '(string? "hi") e) #t))

(test-case "string? negative: (string? 42) returns #f"
  (define e (default-env))
  (check-equal? (straw-eval '(string? 42) e) #f))

(test-case "symbol? positive: (symbol? 'foo) returns #t"
  (define e (default-env))
  (check-equal? (straw-eval '(symbol? (quote foo)) e) #t))

(test-case "symbol? negative: (symbol? 42) returns #f"
  (define e (default-env))
  (check-equal? (straw-eval '(symbol? 42) e) #f))

(test-case "boolean? positive: (boolean? #t) returns #t"
  (define e (default-env))
  (check-equal? (straw-eval '(boolean? #t) e) #t))

(test-case "boolean? negative: (boolean? 0) returns #f"
  (define e (default-env))
  (check-equal? (straw-eval '(boolean? 0) e) #f))

(test-case "procedure? lambda: (procedure? (lambda (x) x)) returns #t"
  (define e (default-env))
  (check-equal? (straw-eval '(procedure? (lambda (x) x)) e) #t))

(test-case "procedure? builtin: (procedure? +) returns #t"
  (define e (default-env))
  (check-equal? (straw-eval '(procedure? +) e) #t))

(test-case "procedure? negative: (procedure? 42) returns #f"
  (define e (default-env))
  (check-equal? (straw-eval '(procedure? 42) e) #f))

;; E1.15 — I/O: display

(test-case "Display string: (display \"hi\") prints hi to stdout and returns void"
  (define e (default-env))
  (check-equal? (with-output-to-string (lambda () (straw-eval '(display "hi") e)))
                "hi")
  (check-equal? (straw-eval '(display "hi") e) (void)))

(test-case "Display number: (display 42) prints 42 to stdout and returns void"
  (define e (default-env))
  (check-equal? (with-output-to-string (lambda () (straw-eval '(display 42) e)))
                "42")
  (check-equal? (straw-eval '(display 42) e) (void)))

;; E1.15 — I/O: newline

(test-case "Newline: (newline) prints newline to stdout and returns void"
  (define e (default-env))
  (check-equal? (with-output-to-string (lambda () (straw-eval '(newline) e)))
                "\n")
  (check-equal? (straw-eval '(newline) e) (void)))
