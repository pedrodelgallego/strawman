# Strawman — Minimal Lisp Interpreter in Racket

## Project Overview
A minimal Lisp interpreter built incrementally in Racket, one layer at a time.

## Build & Run
- **Run REPL**: `racket strawman.rkt`
- **Run tests**: `raco test tests/`
- **Run single test**: `raco test tests/test-lexer.rkt`
- **Run a script**: `racket strawman.rkt <file.straw>`

## Architecture
Source → Tokens → AST → Eval → Result

- `src/lexer.rkt` — Tokenizer
- `src/parser.rkt` — Parser
- `src/env.rkt` — Environments (lexical scope)
- `src/eval.rkt` — Evaluator
- `src/builtins.rkt` — Built-in primitives
- `src/repl.rkt` — REPL
- `strawman.rkt` — Entry point

## Coding Conventions
- `#lang racket` for all source files
- Prefer `match`, `cond`, pattern matching over nested `if`
- Test with `rackunit`; every module gets a test file in `tests/`
- Use `provide`/`require` for module interfaces
- Prefer pure functions; mutation only where necessary (environments)

## Development Approach
Build incrementally — always keep the system runnable:
1. Start with the smallest working piece
2. Add one feature at a time
3. Write tests alongside implementation
4. Each commit should leave the project in a working state
