#lang racket
(provide instruction-set compilation-forms)

;; Strawman Lisp bytecode instruction set definition.
;; Each instruction is an opcode with a fixed format: opcode + operands.
;; See docs/bytecode.md for full specification.

;; instruction-set : hash of opcode-symbol -> hash with 'description and 'operands
(define instruction-set
  (hash
   'CONST
   (hash 'description "Push a constant value onto the stack."
         'operands 1)
   'LOOKUP
   (hash 'description "Look up a variable in the current environment and push its value onto the stack."
         'operands 1)
   'SET
   (hash 'description "Pop the top of the stack and update an existing variable binding in the environment."
         'operands 1)
   'DEFINE
   (hash 'description "Pop the top of the stack and create a new variable binding in the current environment."
         'operands 1)
   'JUMP
   (hash 'description "Unconditionally jump to the instruction at the given offset."
         'operands 1)
   'JUMP-IF-FALSE
   (hash 'description "Pop the top of the stack; if it is #f, jump to the given offset; otherwise continue."
         'operands 1)
   'CALL
   (hash 'description "Pop the procedure and N arguments from the stack, then apply the procedure. The operand is the argument count."
         'operands 1)
   'RETURN
   (hash 'description "Return the top of the stack to the caller, restoring the previous frame."
         'operands 0)
   'CLOSURE
   (hash 'description "Create a closure from the code at the given offset and the current environment, push it onto the stack. Operand is the offset to the closure body code."
         'operands 1)
   'POP
   (hash 'description "Discard the top value from the stack."
         'operands 0)
   'HALT
   (hash 'description "Stop execution and return the top of the stack as the program result."
         'operands 0)))

;; compilation-forms : hash of form-symbol -> string describing the compilation strategy
(define compilation-forms
  (hash
   ;; Literals
   'number "Compile to (CONST <value>)."
   'string "Compile to (CONST <value>)."
   'boolean "Compile to (CONST <value>)."
   'symbol "Compile to (LOOKUP <name>)."
   ;; Core special forms
   'quote "Compile to (CONST <datum>)."
   'if "Compile test, emit JUMP-IF-FALSE to alternative branch, compile consequent, emit JUMP past alternative, compile alternative."
   'begin "Compile each sub-expression in sequence; insert POP between them to discard intermediate values."
   'define "Compile the value expression, then emit (DEFINE <name>)."
   'set! "Compile the value expression, then emit (SET <name>)."
   'lambda "Emit (CLOSURE <offset>) pointing to a compiled body that ends with RETURN."
   'and "Compile first expr; for each subsequent expr emit JUMP-IF-FALSE to end (short-circuit with #f); last expr is tail."
   'or "Compile first expr; for each subsequent expr, duplicate and emit JUMP-IF-FALSE to next; on true short-circuit; last expr is tail."
   'let "Compile each init expression, extend environment with bindings, compile body."
   'let* "Compile and bind each init expression sequentially, extending environment at each step, compile body."
   'letrec "Extend environment with void placeholders, compile each init and SET each binding, compile body."
   'application "Compile the operator, compile each argument left to right, emit (CALL <n>) where n is the argument count."
   ;; Continuations and escape forms
   'catch "Compile tag-expr, set up exception handler frame, compile body, on throw with matching tag push the value."
   'throw "Compile tag-expr, compile value-expr, raise to nearest matching catch handler."
   'call/cc "Compile proc-expr, capture current continuation as a closure, apply proc to it."
   'block "Set up a named escape point, compile body; return-from with matching name jumps here."
   'return-from "Compile value-expr, unwind to the named block escape point."
   'unwind-protect "Compile body with cleanup guard; cleanup runs on both normal and exceptional exit."))
