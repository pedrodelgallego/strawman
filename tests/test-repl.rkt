#lang racket
(require rackunit
         racket/system
         "../src/repl.rkt")

;; Test: Simple expression → prints result
;; E1.16 Test Matrix row 1: input `42` should print `42`
(test-case "simple expression prints result"
  (define input (open-input-string "42\n"))
  (define output (open-output-string))
  (run-repl input output)
  (define out (get-output-string output))
  ;; Output should contain the prompt and the result 42
  (check-true (string-contains? out "42")
              "REPL should print the result of a simple expression"))

;; Test: Computation → prints result
;; E1.16 Test Matrix row 2: input `(+ 1 2)` should print `3`
(test-case "computation prints result"
  (define input (open-input-string "(+ 1 2)\n"))
  (define output (open-output-string))
  (run-repl input output)
  (define out (get-output-string output))
  ;; Output should contain "3" after the prompt
  (check-regexp-match #rx"strawman> 3\n" out))

;; Test: Define then use across inputs
;; E1.16 Test Matrix row 3: `(define x 5)` then `x` → prints nothing, then `5`
(test-case "define then use across inputs"
  (define input (open-input-string "(define x 5)\nx\n"))
  (define output (open-output-string))
  (run-repl input output)
  (define out (get-output-string output))
  ;; After (define x 5), the REPL should print only a prompt (no value).
  ;; After x, the REPL should print 5.
  ;; So the full output should be: "strawman> strawman> 5\nstrawman> "
  (check-equal? out "strawman> strawman> 5\nstrawman> "))

;; Test: Multi-line input (balanced parens)
;; E1.16 Test Matrix row 4: input `(+ 1\n   2)` should print `3`
;; When the first line has unbalanced parens, REPL should show continuation
;; prompt "...      " and keep reading until parens balance.
(test-case "multi-line input with unbalanced parens"
  (define input (open-input-string "(+ 1\n   2)\n"))
  (define output (open-output-string))
  (run-repl input output)
  (define out (get-output-string output))
  ;; Should see: primary prompt, continuation prompt, then result 3
  (check-regexp-match #rx"strawman> " out)
  (check-regexp-match #rx"\\.\\.\\.      " out)
  (check-regexp-match #rx"3\n" out))

;; Test: Error recovery — error then normal expr
;; E1.16 Test Matrix row: `(/ 1 0)` then `42` → prints Error: division by zero, then 42
(test-case "error recovery: error then normal expression"
  (define input (open-input-string "(/ 1 0)\n42\n"))
  (define output (open-output-string))
  (run-repl input output)
  (define out (get-output-string output))
  ;; Should print error for the first expression
  (check-regexp-match #rx"Error: division by zero" out)
  ;; Should continue and evaluate the second expression normally
  ;; The full output should show the error, then a new prompt with 42
  (check-equal? out "strawman> Error: division by zero\nstrawman> 42\nstrawman> "))

;; Test: Unbound variable error
;; E1.16 Test Matrix row: `foo` → prints `Error: unbound variable: foo`
(test-case "unbound variable prints error and continues"
  (define input (open-input-string "foo\n42\n"))
  (define output (open-output-string))
  (run-repl input output)
  (define out (get-output-string output))
  ;; Should print the unbound variable error
  (check-regexp-match #rx"Error: unbound variable: foo" out)
  ;; Should recover and evaluate the next expression
  (check-equal? out "strawman> Error: unbound variable: foo\nstrawman> 42\nstrawman> "))

;; Test: (exit) terminates the REPL
;; E1.16 Test Matrix row: `(exit)` → REPL terminates
(test-case "(exit) terminates the REPL"
  (define input (open-input-string "(exit)\n"))
  (define output (open-output-string))
  (run-repl input output)
  (define out (get-output-string output))
  ;; REPL should print the prompt then terminate — no further prompt
  (check-equal? out "strawman> "))

;; Test: (quit) terminates the REPL
;; E1.16 Test Matrix row: `(quit)` → REPL terminates
(test-case "(quit) terminates the REPL"
  (define input (open-input-string "(quit)\n"))
  (define output (open-output-string))
  (run-repl input output)
  (define out (get-output-string output))
  ;; REPL should print the prompt then terminate — no further prompt
  (check-equal? out "strawman> "))

;; Test: EOF (Ctrl-D) terminates the REPL
;; E1.16 Test Matrix row: EOF → REPL terminates
(test-case "EOF terminates the REPL"
  ;; EOF on empty input — REPL shows one prompt then exits
  (define input (open-input-string ""))
  (define output (open-output-string))
  (run-repl input output)
  (define out (get-output-string output))
  (check-equal? out "strawman> ")
  ;; Verify run-repl returns void (clean termination)
  (check-equal? (void? (run-repl (open-input-string "") (open-output-string))) #t))

;; Test: EOF after some input terminates the REPL gracefully
(test-case "EOF after expression terminates the REPL"
  ;; Process one expression, then encounter EOF — no crash
  (define input (open-input-string "42\n"))
  (define output (open-output-string))
  (run-repl input output)
  (define out (get-output-string output))
  ;; Should print result then prompt, and terminate on EOF
  (check-equal? out "strawman> 42\nstrawman> "))

;; ---- Entry point: strawman.rkt wiring tests ----
;; E1.16 Definition of Done: Entry point `strawman.rkt` wired up:
;;   no args → REPL, file arg → execute

;; Test: run-file executes a .straw file and produces output
(test-case "run-file executes a script file"
  (define tmp (make-temporary-file "straw-test-~a.straw"))
  (display-to-file "(display 42)(newline)" tmp #:exists 'replace)
  (define output (open-output-string))
  (run-file (path->string tmp) output)
  (delete-file tmp)
  (check-equal? (get-output-string output) "42\n"))

;; Test: run-file executes multiple expressions in a file
(test-case "run-file executes multiple expressions"
  (define tmp (make-temporary-file "straw-test-~a.straw"))
  (display-to-file "(define x 10)(display (+ x 5))(newline)" tmp #:exists 'replace)
  (define output (open-output-string))
  (run-file (path->string tmp) output)
  (delete-file tmp)
  (check-equal? (get-output-string output) "15\n"))

;; Test: strawman.rkt with file arg executes the file
(test-case "strawman.rkt with file arg executes script"
  (define tmp (make-temporary-file "straw-test-~a.straw"))
  (display-to-file "(display 99)(newline)" tmp #:exists 'replace)
  (define result
    (with-output-to-string
      (lambda ()
        (parameterize ([current-error-port (current-output-port)])
          (system* (find-executable-path "racket")
                   (path->string (build-path (current-directory) ".." "strawman.rkt"))
                   (path->string tmp))))))
  (delete-file tmp)
  (check-true (string-contains? result "99")))

;; Test: strawman.rkt with no args starts REPL (feed it input via pipe)
(test-case "strawman.rkt with no args starts REPL"
  (define-values (proc stdout stdin stderr)
    (subprocess #f #f #f
                (find-executable-path "racket")
                (path->string (build-path (current-directory) ".." "strawman.rkt"))))
  (display "(+ 1 2)\n" stdin)
  (display "(exit)\n" stdin)
  (close-output-port stdin)
  (define output (port->string stdout))
  (close-input-port stdout)
  (close-input-port stderr)
  (subprocess-wait proc)
  ;; REPL should show prompt and result
  (check-true (string-contains? output "strawman> "))
  (check-true (string-contains? output "3")))
