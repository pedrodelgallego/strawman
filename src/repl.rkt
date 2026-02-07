#lang racket
(require "lexer.rkt"
         "parser.rkt"
         "eval.rkt"
         "env.rkt"
         "builtins.rkt")
(provide run-repl run-file (struct-out exn:straw:exit))

(struct exn:straw:exit exn:fail () #:transparent)

;; paren-depth : string -> integer
;; Count net open parens (open minus close) in a string.
;; Ignores parens inside string literals and comments.
(define (paren-depth str)
  (define len (string-length str))
  (let loop ([i 0] [depth 0] [in-string? #f])
    (cond
      [(>= i len) depth]
      [in-string?
       (define ch (string-ref str i))
       (cond
         [(and (char=? ch #\\) (< (add1 i) len))
          (loop (+ i 2) depth #t)]
         [(char=? ch #\")
          (loop (add1 i) depth #f)]
         [else (loop (add1 i) depth #t)])]
      [else
       (define ch (string-ref str i))
       (cond
         [(char=? ch #\;)
          ;; Skip to end of line
          (let skip ([j (add1 i)])
            (cond
              [(>= j len) (loop j depth #f)]
              [(char=? (string-ref str j) #\newline) (loop (add1 j) depth #f)]
              [else (skip (add1 j))]))]
         [(char=? ch #\") (loop (add1 i) depth #t)]
         [(char=? ch #\() (loop (add1 i) (add1 depth) #f)]
         [(char=? ch #\)) (loop (add1 i) (sub1 depth) #f)]
         [else (loop (add1 i) depth #f)])])))

;; run-repl : [input-port] [output-port] -> void
;; Read-eval-print loop. Reads from input, writes to output.
(define (run-repl [in (current-input-port)] [out (current-output-port)])
  (define env (default-env))
  (env-set! env 'exit (lambda ()
                        (raise (exn:straw:exit "exit" (current-continuation-marks)))))
  (env-set! env 'quit (lambda ()
                        (raise (exn:straw:exit "quit" (current-continuation-marks)))))
  (let loop ()
    (fprintf out "strawman> ")
    (flush-output out)
    (define line (read-line in))
    (cond
      [(eof-object? line) (void)]
      [else
       ;; Accumulate lines until parens are balanced
       (define input
         (let accum ([buf line])
           (if (> (paren-depth buf) 0)
               (begin
                 (fprintf out "...      ")
                 (flush-output out)
                 (let ([next (read-line in)])
                   (if (eof-object? next)
                       buf
                       (accum (string-append buf "\n" next)))))
               buf)))
       (define should-exit?
         (with-handlers ([exn:straw:exit? (lambda (e) #t)]
                         [exn:fail? (lambda (e)
                                      (fprintf out "Error: ~a\n" (exn-message e))
                                      #f)])
           (define tokens (tokenize input))
           (when (not (null? tokens))
             (define-values (expr remaining) (parse tokens))
             (define result (straw-eval expr env))
             (unless (void? result)
               (fprintf out "~a\n" result)))
           #f))
       (unless should-exit? (loop))])))

;; run-file : string [output-port] -> void
;; Read and execute all expressions from the given file path.
(define (run-file path [out (current-output-port)])
  (define source (file->string path))
  (define exprs (read-all-from-string source))
  (define env (default-env))
  (parameterize ([current-output-port out])
    (for ([expr (in-list exprs)])
      (straw-eval expr env))))
