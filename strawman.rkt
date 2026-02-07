#lang racket
(require "src/repl.rkt")

(define args (vector->list (current-command-line-arguments)))
(define compiled? (member "--compiled" args))
(define file-args (filter (lambda (a) (not (string=? a "--compiled"))) args))
(cond
  [(null? file-args) (run-repl #:compiled? (and compiled? #t))]
  [else (run-file (car file-args))])
