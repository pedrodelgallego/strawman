#lang racket
(require "lexer.rkt")
(provide parse read-from-string read-all-from-string)

;; parse : (listof token) -> (values s-expr (listof token))
;; Consume one s-expression from tokens, return it and remaining tokens.
(define (parse tokens)
  (match tokens
    ['() (error "empty input")]
    [(cons (token 'NUMBER v) rest) (values v rest)]
    [(cons (token 'SYMBOL v) rest) (values v rest)]
    [(cons (token 'STRING v) rest) (values v rest)]
    [(cons (token 'BOOLEAN v) rest) (values v rest)]
    [(cons (token 'LPAREN _) rest) (parse-list rest)]
    [(cons (token 'RPAREN _) _) (error "unexpected closing paren")]
    [(cons (token 'QUOTE _) rest)
     (define-values (quoted remaining) (parse rest))
     (values (list 'quote quoted) remaining)]
    [(cons (token 'BACKTICK _) rest)
     (define-values (quoted remaining) (parse rest))
     (values (list 'quasiquote quoted) remaining)]
    [(cons (token 'COMMA _) rest)
     (define-values (unquoted remaining) (parse rest))
     (values (list 'unquote unquoted) remaining)]
    [(cons (token 'COMMA-SPLICE _) rest)
     (define-values (unquoted remaining) (parse rest))
     (values (list 'unquote-splicing unquoted) remaining)]
    [_ (error "parse: unexpected token")]))

;; parse-list : (listof token) -> (values list (listof token))
;; Parse elements until RPAREN, return list and remaining tokens.
(define (parse-list tokens)
  (match tokens
    ['() (error "unexpected end of input")]
    [(cons (token 'RPAREN _) rest) (values '() rest)]
    [_
     (define-values (first remaining) (parse tokens))
     (define-values (rest-elems after) (parse-list remaining))
     (values (cons first rest-elems) after)]))

;; read-from-string : string -> s-expr
;; Convenience: tokenize then parse a single expression.
(define (read-from-string str)
  (define tokens (tokenize str))
  (when (null? tokens)
    (error "empty input"))
  (define-values (expr remaining) (parse tokens))
  expr)

;; read-all-from-string : string -> (listof s-expr)
;; Parse all top-level expressions from a string.
(define (read-all-from-string str)
  (define tokens (tokenize str))
  (let loop ([toks tokens] [exprs '()])
    (if (null? toks)
        (reverse exprs)
        (let-values ([(expr remaining) (parse toks)])
          (loop remaining (cons expr exprs))))))
