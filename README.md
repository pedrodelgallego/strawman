# Strawman

A minimal Lisp interpreter written in Racket.

## Why?

Building a Lisp interpreter is one of the most illuminating exercises in
computer science. By constructing one from scratch — lexer, parser, evaluator,
environment — you confront the fundamental ideas that underpin all programming
languages: how expressions become values, how names acquire meaning, and how
functions capture their surroundings.

Strawman is deliberately small. The goal is not to build a production language
but to arrive at the smallest complete thing: a Lisp that can define functions,
close over variables, recurse, and manipulate lists. Nothing more, nothing less.

## How?

Strawman is built entirely through conversation with an AI coding assistant
(Claude Code), without the author writing any code by hand. Every line of
Racket in this repo was generated via prompts, reviewed, and committed — a
workflow sometimes called the **Ralph approach**: direct the architecture,
describe the intent, and let the machine produce the implementation.

This is as much an experiment in AI-assisted development as it is a Lisp
interpreter.

## What it supports

- Arithmetic and comparisons
- First-class functions and closures
- Lexical scoping
- Lists and cons cells
- A REPL for interactive exploration

## Quick Start

```bash
# Start the REPL
racket strawman.rkt

# Run a script
racket strawman.rkt examples/hello.straw
```

## Example

```lisp
(define square (lambda (x) (* x x)))
(square 5)
; => 25

(define factorial
  (lambda (n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1))))))
(factorial 10)
; => 3628800
```

## Running Tests

```bash
raco test tests/
```
