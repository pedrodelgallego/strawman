#lang racket
(require rackunit
         "../src/parser.rkt")

(test-case "Single atom: number"
  (check-equal? (read-from-string "42") 42))

(test-case "Single atom: symbol"
  (check-equal? (read-from-string "foo") 'foo))

(test-case "Single atom: string"
  (check-equal? (read-from-string "\"hi\"") "hi"))

(test-case "Single atom: boolean"
  (check-equal? (read-from-string "#t") #t))

(test-case "Simple list: (+ 1 2)"
  (check-equal? (read-from-string "(+ 1 2)") '(+ 1 2)))

(test-case "Nested list: (+ (* 2 3) 4)"
  (check-equal? (read-from-string "(+ (* 2 3) 4)") '(+ (* 2 3) 4)))

(test-case "Empty list: ()"
  (check-equal? (read-from-string "()") '()))

(test-case "Deeply nested: (a (b (c d)))"
  (check-equal? (read-from-string "(a (b (c d)))") '(a (b (c d)))))

(test-case "Quote sugar: 'x → (quote x)"
  (check-equal? (read-from-string "'x") '(quote x)))

(test-case "Quote sugar: '(1 2 3) → (quote (1 2 3))"
  (check-equal? (read-from-string "'(1 2 3)") '(quote (1 2 3))))

(test-case "Multiple expressions via read-all-from-string"
  (check-equal? (read-all-from-string "1 2 3") '(1 2 3)))

(test-case "Error: unmatched open paren"
  (check-exn #rx"unexpected end of input"
             (lambda () (read-from-string "(+ 1"))))

(test-case "Error: unmatched close paren"
  (check-exn #rx"unexpected closing paren"
             (lambda () (read-from-string ")"))))

(test-case "Error: empty input"
  (check-exn #rx"empty input"
             (lambda () (read-from-string ""))))
