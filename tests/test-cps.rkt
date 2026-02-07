#lang racket
(require rackunit
         "../src/eval.rkt"
         "../src/builtins.rkt")

;; E3.1 — CPS evaluator: identity continuation
;; Test Matrix row: (+ 1 2) with identity k → 3
(test-case "Identity continuation: (+ 1 2) with identity k returns 3"
  (define e (default-env))
  ;; straw-eval in CPS mode takes (expr env continuation)
  ;; The identity continuation just returns its argument.
  (check-equal? (straw-eval '(+ 1 2) e (lambda (v) v)) 3))

;; ─── E3.2 — call/cc ──────────────────────────────────────────────────

;; Test Matrix row: Normal return
(test-case "call/cc normal return: (call/cc (lambda (k) 42)) returns 42"
  (define e (default-env))
  (check-equal? (straw-eval '(call/cc (lambda (k) 42)) e) 42))

;; Test Matrix row: Early exit
(test-case "call/cc early exit: (call/cc (lambda (k) (k 10) 20)) returns 10"
  (define e (default-env))
  (check-equal? (straw-eval '(call/cc (lambda (k) (k 10) 20)) e) 10))

;; Test Matrix row: In expression
(test-case "call/cc in expression: (+ 1 (call/cc (lambda (k) (k 5)))) returns 6"
  (define e (default-env))
  (check-equal? (straw-eval '(+ 1 (call/cc (lambda (k) (k 5)))) e) 6))

;; Test Matrix row: Saved continuation
;; With true call/cc, invoking a saved continuation re-enters the original
;; computation. We test this within a single straw-eval call by using a
;; counter to distinguish the first pass (returns normally) from the second
;; (invoked via saved continuation).
(test-case "call/cc saved continuation: save k, call later"
  (define e (default-env))
  ;; result collects what (+ 1 ...) produces on each pass.
  ;; First pass: call/cc returns 10 normally → (+ 1 10) = 11, saved into result.
  ;; count tracks how many times we've been through; we invoke (saved 20) once.
  ;; Second pass: call/cc "returns" 20 → (+ 1 20) = 21.
  (check-equal?
    (straw-eval '(begin
                   (define saved #f)
                   (define count 0)
                   (define result
                     (+ 1 (call/cc (lambda (k) (set! saved k) 10))))
                   (if (= count 0)
                       (begin (set! count 1) (saved 20))
                       result))
                e)
    21))

;; Test Matrix row: Non-procedure error
(test-case "call/cc non-procedure: (call/cc 42) raises error"
  (define e (default-env))
  (check-exn #rx"call/cc: expected procedure"
             (lambda () (straw-eval '(call/cc 42) e))))

;; Acceptance criteria: (+ 1 (call/cc (lambda (k) (+ 2 (k 3))))) => 4
(test-case "call/cc acceptance: (+ 1 (call/cc (lambda (k) (+ 2 (k 3))))) returns 4"
  (define e (default-env))
  (check-equal?
    (straw-eval '(+ 1 (call/cc (lambda (k) (+ 2 (k 3))))) e)
    4))

;; ─── E3.3 — catch / throw ──────────────────────────────────────────

;; Test Matrix row: No throw
(test-case "catch no throw: (catch 'x 42) returns 42"
  (define e (default-env))
  (check-equal? (straw-eval '(catch 'x 42) e) 42))

;; Test Matrix row: Simple throw
(test-case "catch simple throw: (catch 'x (begin (throw 'x 10) 20)) returns 10"
  (define e (default-env))
  (check-equal? (straw-eval '(catch 'x (begin (throw 'x 10) 20)) e) 10))

;; Test Matrix row: Nested catch
(test-case "catch nested: (catch 'a (catch 'b (throw 'a 1))) returns 1"
  (define e (default-env))
  (check-equal? (straw-eval '(catch 'a (catch 'b (throw 'a 1))) e) 1))

;; Test Matrix row: Wrong tag
(test-case "catch wrong tag: (catch 'a (throw 'b 1)) raises error"
  (define e (default-env))
  (check-exn #rx"no matching catch for tag"
             (lambda () (straw-eval '(catch 'a (throw 'b 1)) e))))

;; Test Matrix row: Throw in function
(test-case "catch throw in function: (catch 'x ((lambda () (throw 'x 99)))) returns 99"
  (define e (default-env))
  (check-equal? (straw-eval '(catch 'x ((lambda () (throw 'x 99)))) e) 99))

;; Acceptance criteria
(test-case "catch acceptance: (catch 'err (begin (throw 'err \"oops\") \"unreachable\")) returns \"oops\""
  (define e (default-env))
  (check-equal? (straw-eval '(catch 'err (begin (throw 'err "oops") "unreachable")) e) "oops"))

;; ─── E3.4 — block / return-from ──────────────────────────────────────

;; Test Matrix row: No return
(test-case "block no return: (block b 42) returns 42"
  (define e (default-env))
  (check-equal? (straw-eval '(block b 42) e) 42))

;; Test Matrix row: Early return
(test-case "block early return: (block b (return-from b 10) 20) returns 10"
  (define e (default-env))
  (check-equal? (straw-eval '(block b (return-from b 10) 20) e) 10))

;; Test Matrix row: Nested blocks
(test-case "block nested: (block a (block b (return-from a 1))) returns 1"
  (define e (default-env))
  (check-equal? (straw-eval '(block a (block b (return-from a 1))) e) 1))

;; Test Matrix row: Return from inner
(test-case "block return from inner: (block a (block b (return-from b 2)) 3) returns 3"
  (define e (default-env))
  (check-equal? (straw-eval '(block a (block b (return-from b 2)) 3) e) 3))

;; Test Matrix row: Unknown block
(test-case "block unknown: (return-from z 1) raises error"
  (define e (default-env))
  (check-exn #rx"unknown block"
             (lambda () (straw-eval '(return-from z 1) e))))

;; Acceptance criteria
(test-case "block acceptance: (block done (return-from done 42) (error \"unreachable\")) returns 42"
  (define e (default-env))
  (check-equal? (straw-eval '(block done (return-from done 42) (error "unreachable")) e) 42))

;; ─── E3.5 — unwind-protect ──────────────────────────────────────────

;; Test Matrix row: Normal
(test-case "unwind-protect normal: (unwind-protect 42 (display \"clean\")) returns 42, prints clean"
  (define e (default-env))
  (define output (with-output-to-string
                   (lambda () (check-equal? (straw-eval '(unwind-protect 42 (display "clean")) e) 42))))
  (check-equal? output "clean"))

;; Test Matrix row: With throw
(test-case "unwind-protect with throw: cleanup runs before throw propagates"
  (define e (default-env))
  (define output (with-output-to-string
                   (lambda ()
                     (check-equal?
                       (straw-eval '(catch 'x (unwind-protect (throw 'x 1) (display "clean"))) e)
                       1))))
  (check-equal? output "clean"))

;; Test Matrix row: Cleanup order
(test-case "unwind-protect cleanup order: multiple cleanup forms run in order"
  (define e (default-env))
  (define output (with-output-to-string
                   (lambda ()
                     (check-equal?
                       (straw-eval '(unwind-protect 10 (display "a") (display "b")) e)
                       10))))
  (check-equal? output "ab"))

;; Acceptance criteria
(test-case "unwind-protect acceptance: (catch 'e (unwind-protect (throw 'e \"err\") (display \"cleaned\"))) returns \"err\""
  (define e (default-env))
  (define output (with-output-to-string
                   (lambda ()
                     (check-equal?
                       (straw-eval '(catch 'e (unwind-protect (throw 'e "err") (display "cleaned"))) e)
                       "err"))))
  (check-equal? output "cleaned"))
