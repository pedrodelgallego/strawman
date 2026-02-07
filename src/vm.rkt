#lang racket
(require "env.rkt")
(provide vm-execute)

;; A VM closure: captures the code vector, body offset, param names, and environment.
(struct vm-closure (code body-offset params env) #:transparent)

;; A call frame: saves the return address (PC) and environment.
(struct frame (pc env) #:transparent)

;; vm-execute : (listof instruction) × env -> value
;; Execute compiled bytecode on a stack-based virtual machine.
;; The code is a vector of instructions; each instruction is a list (OPCODE operand ...).
(define (vm-execute code-list env)
  (define code (list->vector code-list))
  (define code-len (vector-length code))
  (let loop ([pc 0]
             [stack '()]
             [env env]
             [call-stack '()])
    (when (>= pc code-len)
      ;; If we've run past the end, return top of stack (implicit HALT)
      (if (null? stack)
          (error "vm: stack underflow at end of code")
          (car stack)))
    (if (>= pc code-len)
        (car stack)
        (let ([instr (vector-ref code pc)])
          (define opcode (car instr))
          (match opcode
            ['CONST
             (define val (cadr instr))
             (loop (add1 pc) (cons val stack) env call-stack)]

            ['LOOKUP
             (define name (cadr instr))
             (define val (env-lookup env name))
             (loop (add1 pc) (cons val stack) env call-stack)]

            ['SET
             (when (null? stack)
               (error "vm: stack underflow on SET"))
             (define name (cadr instr))
             (define val (car stack))
             (env-update! env name val)
             (loop (add1 pc) (cons (void) (cdr stack)) env call-stack)]

            ['DEFINE
             (when (null? stack)
               (error "vm: stack underflow on DEFINE"))
             (define name (cadr instr))
             (define val (car stack))
             (env-set! env name val)
             (loop (add1 pc) (cons (void) (cdr stack)) env call-stack)]

            ['JUMP
             (define target (cadr instr))
             (loop target stack env call-stack)]

            ['JUMP-IF-FALSE
             (when (null? stack)
               (error "vm: stack underflow on JUMP-IF-FALSE"))
             (define val (car stack))
             (define target (cadr instr))
             (if (eq? val #f)
                 (loop target (cdr stack) env call-stack)
                 (loop (add1 pc) (cdr stack) env call-stack))]

            ['CALL
             (define n (cadr instr))
             ;; Stack layout: arg1 arg2 ... argN proc (proc is deepest, pushed first)
             ;; Actually the compiler pushes: proc, arg1, arg2, ..., argN
             ;; So on the stack (top first): argN, ..., arg2, arg1, proc
             (when (< (length stack) (+ n 1))
               (error "vm: stack underflow on CALL"))
             (define-values (arg-vals rest) (split-at stack n))
             (define proc (car rest))
             (define remaining-stack (cdr rest))
             (define args (reverse arg-vals))
             (cond
               [(vm-closure? proc)
                ;; Save current frame and jump to closure body
                (define call-env (env-extend (vm-closure-env proc)
                                             (vm-closure-params proc)
                                             args))
                (loop (vm-closure-body-offset proc)
                      remaining-stack
                      call-env
                      (cons (frame (add1 pc) env) call-stack))]
               [(procedure? proc)
                ;; Builtin: apply directly and push result
                (define result (apply proc args))
                (loop (add1 pc) (cons result remaining-stack) env call-stack)]
               [else
                (error (format "not a procedure: ~a" proc))])]

            ['RETURN
             (when (null? stack)
               (error "vm: stack underflow on RETURN"))
             (when (null? call-stack)
               (error "vm: call stack underflow on RETURN"))
             (define val (car stack))
             (define saved-frame (car call-stack))
             (loop (frame-pc saved-frame)
                   (cons val (cdr stack))
                   (frame-env saved-frame)
                   (cdr call-stack))]

            ['CLOSURE
             (define body-offset (cadr instr))
             (define params (caddr instr))
             (define cl (vm-closure code body-offset params env))
             (loop (add1 pc) (cons cl stack) env call-stack)]

            ['POP
             (when (null? stack)
               (error "vm: stack underflow on POP"))
             (loop (add1 pc) (cdr stack) env call-stack)]

            ['HALT
             (if (null? stack)
                 (void)
                 (car stack))]

            [_
             (error (format "vm: invalid opcode: ~a" opcode))])))))
