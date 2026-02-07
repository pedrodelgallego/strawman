#lang racket
(require rackunit
         "../src/bytecode.rkt")

;; E7.1 — Instruction set design
;; Tests that the bytecode instruction set is properly defined

;; The instruction set must include all required opcodes
(test-case "instruction-set contains all required opcodes"
  (define required-opcodes
    '(CONST LOOKUP SET DEFINE JUMP JUMP-IF-FALSE CALL RETURN CLOSURE POP HALT))
  (for ([op (in-list required-opcodes)])
    (check-true (hash-has-key? instruction-set op)
                (format "missing opcode: ~a" op))))

;; Each instruction must have a description (semantics)
(test-case "each instruction has defined semantics"
  (for ([(op info) (in-hash instruction-set)])
    (check-true (string? (hash-ref info 'description #f))
                (format "~a missing description" op))
    (check-true (> (string-length (hash-ref info 'description "")) 0)
                (format "~a has empty description" op))))

;; Each instruction must declare its operand count
(test-case "each instruction declares operand count"
  (for ([(op info) (in-hash instruction-set)])
    (check-true (and (integer? (hash-ref info 'operands #f))
                         (>= (hash-ref info 'operands) 0))
                (format "~a missing operand count" op))))

;; Every core form must be covered in the compilation-forms table
(test-case "compilation-forms covers all core forms"
  (define required-forms
    '(quote if begin define set! lambda and or let let* letrec application
      number string boolean symbol
      catch throw call/cc block return-from unwind-protect))
  (for ([form (in-list required-forms)])
    (check-true (hash-has-key? compilation-forms form)
                (format "missing compilation form: ~a" form))))
