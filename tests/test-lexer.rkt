#lang racket

(require rackunit
         "../src/lexer.rkt")

(test-case "tokenize empty input returns empty list"
  (check-equal? (tokenize "") '()))

(test-case "tokenize single integer"
  (check-equal? (tokenize "42") (list (token 'NUMBER 42))))

(test-case "tokenize negative number"
  (check-equal? (tokenize "-3") (list (token 'NUMBER -3))))

(test-case "tokenize float"
  (check-equal? (tokenize "3.14") (list (token 'NUMBER 3.14))))

(test-case "tokenize string literal"
  (check-equal? (tokenize "\"hello\"") (list (token 'STRING "hello"))))

(test-case "tokenize string with escape"
  (check-equal? (tokenize "\"a\\\"b\"") (list (token 'STRING "a\"b"))))

(test-case "tokenize symbol"
  (check-equal? (tokenize "foo") (list (token 'SYMBOL 'foo))))

(test-case "tokenize operator symbol"
  (check-equal? (tokenize "+") (list (token 'SYMBOL '+))))

(test-case "tokenize boolean true"
  (check-equal? (tokenize "#t") (list (token 'BOOLEAN #t))))

(test-case "tokenize boolean false"
  (check-equal? (tokenize "#f") (list (token 'BOOLEAN #f))))

(test-case "tokenize parentheses"
  (check-equal? (tokenize "()")
                (list (token 'LPAREN "(") (token 'RPAREN ")"))))

(test-case "tokenize mixed expression (+ 1 2)"
  (check-equal? (tokenize "(+ 1 2)")
                (list (token 'LPAREN "(")
                      (token 'SYMBOL '+)
                      (token 'NUMBER 1)
                      (token 'NUMBER 2)
                      (token 'RPAREN ")"))))

(test-case "comment skips to end of line"
  (check-equal? (tokenize "42 ; ignore")
                (list (token 'NUMBER 42))))

(test-case "comment with tokens after newline"
  (check-equal? (tokenize "42 ; this is ignored\n7")
                (list (token 'NUMBER 42) (token 'NUMBER 7))))

(test-case "whitespace only input returns empty list"
  (check-equal? (tokenize "   \n\t  ") '()))

(test-case "unterminated string raises error"
  (check-exn #rx"unterminated string"
             (lambda () (tokenize "\"abc"))))

(test-case "nested parens"
  (check-equal? (tokenize "((a))")
                (list (token 'LPAREN "(")
                      (token 'LPAREN "(")
                      (token 'SYMBOL 'a)
                      (token 'RPAREN ")")
                      (token 'RPAREN ")"))))
