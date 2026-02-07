# Strawman Lisp Bytecode Instruction Set

## Overview

The Strawman bytecode is a stack-based instruction set. The virtual machine
maintains:

- **Value stack** — operands and intermediate results
- **Environment** — lexical variable bindings
- **Program counter (PC)** — index into the instruction sequence
- **Call stack** — saved frames for function calls (return address, environment)

Each instruction has a fixed format: an **opcode** followed by zero or one
**operands**. Operands are inline in the instruction stream.

## Instructions

### CONST *value*

Push a constant value onto the stack.

| Field    | Description          |
|----------|----------------------|
| Opcode   | `CONST`              |
| Operands | 1 — the literal value |

**Stack effect:** `… → … value`

---

### LOOKUP *name*

Look up a variable in the current environment and push its value.

| Field    | Description                |
|----------|----------------------------|
| Opcode   | `LOOKUP`                   |
| Operands | 1 — the variable name/address |

**Stack effect:** `… → … value`

Raises an error if the variable is unbound.

---

### SET *name*

Pop the top of the stack and update an existing variable binding.

| Field    | Description                |
|----------|----------------------------|
| Opcode   | `SET`                      |
| Operands | 1 — the variable name/address |

**Stack effect:** `… value → …`

Raises an error if the variable is not already bound.

---

### DEFINE *name*

Pop the top of the stack and create a new binding in the current environment.

| Field    | Description                |
|----------|----------------------------|
| Opcode   | `DEFINE`                   |
| Operands | 1 — the variable name      |

**Stack effect:** `… value → …`

---

### JUMP *offset*

Unconditionally set the program counter to the given offset.

| Field    | Description             |
|----------|-------------------------|
| Opcode   | `JUMP`                  |
| Operands | 1 — target PC offset    |

**Stack effect:** `… → …` (no change)

---

### JUMP-IF-FALSE *offset*

Pop the top of the stack. If it is `#f`, jump to the given offset.
Otherwise, continue to the next instruction.

| Field    | Description             |
|----------|-------------------------|
| Opcode   | `JUMP-IF-FALSE`         |
| Operands | 1 — target PC offset    |

**Stack effect:** `… value → …`

---

### CALL *n*

Pop the procedure and *n* arguments from the stack, then apply the
procedure. The arguments are pushed first (left to right), then the
procedure.

| Field    | Description                    |
|----------|--------------------------------|
| Opcode   | `CALL`                         |
| Operands | 1 — argument count *n*         |

**Stack effect:** `… arg₁ arg₂ … argₙ proc → … result`

For closures: saves the current frame (PC, environment) on the call stack,
extends the closure's environment with parameter bindings, and jumps to the
closure body. For builtins: applies the Racket procedure directly.

---

### RETURN

Return from the current function. Pops the top of the stack as the return
value, restores the caller's frame (PC, environment) from the call stack,
and pushes the return value onto the caller's stack.

| Field    | Description |
|----------|-------------|
| Opcode   | `RETURN`    |
| Operands | 0           |

**Stack effect:** `… value → (caller's stack) … value`

---

### CLOSURE *offset*

Create a closure capturing the current environment and the code beginning
at *offset*. Push the closure onto the stack.

| Field    | Description                         |
|----------|-------------------------------------|
| Opcode   | `CLOSURE`                           |
| Operands | 1 — offset to the closure body code |

**Stack effect:** `… → … closure`

---

### POP

Discard the top value from the stack.

| Field    | Description |
|----------|-------------|
| Opcode   | `POP`       |
| Operands | 0           |

**Stack effect:** `… value → …`

---

### HALT

Stop execution and return the top of the stack as the final program result.

| Field    | Description |
|----------|-------------|
| Opcode   | `HALT`      |
| Operands | 0           |

**Stack effect:** `… value → value` (result)

---

## Compilation of Core Forms

This section shows how each Strawman source form compiles to bytecode
instructions.

### Literals

| Form       | Compilation                |
|------------|----------------------------|
| Number     | `(CONST <value>)`          |
| String     | `(CONST <value>)`          |
| Boolean    | `(CONST <value>)`          |
| Symbol ref | `(LOOKUP <name>)`          |

### quote

```
(quote <datum>)  →  (CONST <datum>)
```

### if

```
(if test consequent alternative)
→
  <compile test>
  (JUMP-IF-FALSE L1)
  <compile consequent>
  (JUMP L2)
L1:
  <compile alternative>
L2:
```

Two-armed `if` without alternative compiles the same way but with
`(CONST void)` in the alternative branch.

### begin

```
(begin e1 e2 ... en)
→
  <compile e1>
  (POP)
  <compile e2>
  (POP)
  ...
  <compile en>        ; last value stays on stack
```

### define

```
(define x <expr>)
→
  <compile expr>
  (DEFINE x)
```

Function shorthand `(define (f x) body)` desugars to
`(define f (lambda (x) body))`.

### set!

```
(set! x <expr>)
→
  <compile expr>
  (SET x)
```

### lambda

```
(lambda (params ...) body ...)
→
  (CLOSURE L1)
  (JUMP L2)        ; skip over the body
L1:
  <compile body>   ; with params bound in extended env
  (RETURN)
L2:
```

### and

```
(and e1 e2 ... en)
→
  <compile e1>
  (JUMP-IF-FALSE Lend)   ; short-circuit: result is #f
  <compile e2>
  (JUMP-IF-FALSE Lend)
  ...
  <compile en>           ; last value is the result
  (JUMP Ldone)
Lend:
  (CONST #f)
Ldone:
```

`(and)` with no arguments compiles to `(CONST #t)`.

### or

```
(or e1 e2 ... en)
→
  <compile e1>
  (JUMP-IF-FALSE L2)    ; false → try next
  (JUMP Ldone)          ; true → short-circuit
L2:
  <compile e2>
  (JUMP-IF-FALSE L3)
  (JUMP Ldone)
...
Ln:
  <compile en>           ; last value is the result
Ldone:
```

`(or)` with no arguments compiles to `(CONST #f)`.

### let

```
(let ((x1 e1) (x2 e2)) body)
→
  <compile e1>
  <compile e2>
  ; extend env with x1, x2
  <compile body>
```

Implemented as an application of an anonymous lambda:

```
  <compile e1>
  <compile e2>
  (CLOSURE Lbody)      ; (lambda (x1 x2) body)
  (CALL 2)
```

### let*

```
(let* ((x1 e1) (x2 e2)) body)
→
  <compile e1>           ; bind x1
  <compile e2>           ; x1 visible here, bind x2
  <compile body>         ; x1, x2 visible
```

Sequential binding: each init is compiled with the previous bindings
in scope, equivalent to nested `let`.

### letrec

```
(letrec ((x1 e1) (x2 e2)) body)
→
  (CONST void)
  (DEFINE x1)
  (CONST void)
  (DEFINE x2)
  <compile e1>
  (SET x1)
  <compile e2>
  (SET x2)
  <compile body>
```

### Application

```
(f arg1 arg2 ... argn)
→
  <compile f>
  <compile arg1>
  <compile arg2>
  ...
  <compile argn>
  (CALL n)
```

### catch

```
(catch tag-expr body-expr)
→
  <compile tag-expr>
  ; set up exception handler frame
  <compile body-expr>
  ; on throw with matching tag, push value
```

### throw

```
(throw tag-expr value-expr)
→
  <compile tag-expr>
  <compile value-expr>
  ; raise to nearest matching catch handler
```

### call/cc

```
(call/cc proc-expr)
→
  <compile proc-expr>
  ; capture current continuation as closure, apply proc to it
```

### block

```
(block name body ...)
→
  ; set up named escape point
  <compile body>
  ; return-from with matching name jumps here
```

### return-from

```
(return-from name value-expr)
→
  <compile value-expr>
  ; unwind to named block escape point
```

### unwind-protect

```
(unwind-protect body cleanup ...)
→
  ; compile body with cleanup guard
  <compile body>
  ; cleanup runs on both normal and exceptional exit
  <compile cleanup>
```

## Completeness

Every core form in the Strawman evaluator (`straw-eval`) can be expressed
as a sequence of the 11 instructions defined above. The instruction set is
sufficient to compile:

- All literal types (numbers, strings, booleans)
- Variable reference and mutation
- Conditional branching
- Sequential execution
- Function definition and application
- Lexical scoping (let, let*, letrec)
- Short-circuit boolean operators
- Continuation and escape forms
