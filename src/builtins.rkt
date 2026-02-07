#lang racket
(require "env.rkt"
         "eval.rkt")
(provide default-env)

(define (straw-add . args)
  (for ([a args])
    (unless (number? a)
      (error "expected number")))
  (apply + args))

(define (straw-sub . args)
  (for ([a args])
    (unless (number? a)
      (error "expected number")))
  (apply - args))

(define (straw-mul . args)
  (for ([a args])
    (unless (number? a)
      (error "expected number")))
  (apply * args))

(define (straw-div . args)
  (for ([a args])
    (unless (number? a)
      (error "expected number")))
  (when (< (length args) 2)
    (error "arity mismatch"))
  (for ([a (cdr args)])
    (when (zero? a)
      (error "division by zero")))
  (define result (apply / args))
  (if (exact? result)
      (if (integer? result)
          result
          (exact->inexact result))
      result))

(define (straw-mod a b)
  (unless (number? a) (error "expected number"))
  (unless (number? b) (error "expected number"))
  (when (zero? b) (error "division by zero"))
  (modulo a b))

(define (make-numeric-comparator op)
  (lambda (a b)
    (unless (number? a) (error "expected number"))
    (unless (number? b) (error "expected number"))
    (op a b)))

(define straw-< (make-numeric-comparator <))
(define straw-> (make-numeric-comparator >))
(define straw-<= (make-numeric-comparator <=))
(define straw->= (make-numeric-comparator >=))
(define straw-= (make-numeric-comparator =))

(define (straw-eq? a b)
  (eq? a b))

(define (straw-equal? a b)
  (equal? a b))

(define (straw-cons a b)
  (mcons a b))

(define (straw-car p)
  (cond
    [(mpair? p) (mcar p)]
    [(pair? p)  (car p)]
    [else (error "car: expected pair")]))

(define (straw-cdr p)
  (cond
    [(mpair? p) (mcdr p)]
    [(pair? p)  (cdr p)]
    [else (error "cdr: expected pair")]))

(define (straw-list . args)
  (foldr mcons '() args))

(define (straw-null? x)
  (null? x))

(define (straw-pair? x)
  (or (pair? x) (mpair? x)))

(define (straw-set-car! p v)
  (unless (mpair? p)
    (error "set-car!: expected mutable pair"))
  (set-mcar! p v)
  (void))

(define (straw-set-cdr! p v)
  (unless (mpair? p)
    (error "set-cdr!: expected mutable pair"))
  (set-mcdr! p v)
  (void))

(define (straw-not x)
  (eq? x #f))

(define (straw-number? x) (number? x))
(define (straw-string? x) (string? x))
(define (straw-symbol? x) (symbol? x))
(define (straw-boolean? x) (boolean? x))
(define (straw-procedure? x) (or (closure? x) (procedure? x)))

;; Vectors
(define (straw-make-vector . args)
  (match args
    [(list n)
     (unless (and (integer? n) (>= n 0))
       (error "make-vector: expected non-negative integer"))
     (make-vector n 0)]
    [(list n fill)
     (unless (and (integer? n) (>= n 0))
       (error "make-vector: expected non-negative integer"))
     (make-vector n fill)]
    [_ (error "make-vector: expected 1 or 2 arguments")]))

(define (straw-vector-ref v i)
  (unless (vector? v)
    (error "vector-ref: expected vector"))
  (unless (and (integer? i) (>= i 0) (< i (vector-length v)))
    (error "vector-ref: index out of range"))
  (vector-ref v i))

(define (straw-vector-set! v i val)
  (unless (vector? v)
    (error "vector-set!: expected vector"))
  (unless (and (integer? i) (>= i 0) (< i (vector-length v)))
    (error "vector-set!: index out of range"))
  (vector-set! v i val)
  (void))

(define (straw-vector-length v)
  (unless (vector? v)
    (error "vector-length: expected vector"))
  (vector-length v))

(define (straw-vector? x)
  (vector? x))

(define (straw-display x)
  (display x)
  (void))

(define (straw-newline)
  (newline)
  (void))

(define (default-env)
  (define e (make-env))
  (env-set! e '+ straw-add)
  (env-set! e '- straw-sub)
  (env-set! e '* straw-mul)
  (env-set! e '/ straw-div)
  (env-set! e 'mod straw-mod)
  (env-set! e '< straw-<)
  (env-set! e '> straw->)
  (env-set! e '<= straw-<=)
  (env-set! e '>= straw->=)
  (env-set! e '= straw-=)
  (env-set! e 'eq? straw-eq?)
  (env-set! e 'equal? straw-equal?)
  (env-set! e 'cons straw-cons)
  (env-set! e 'car straw-car)
  (env-set! e 'cdr straw-cdr)
  (env-set! e 'list straw-list)
  (env-set! e 'null? straw-null?)
  (env-set! e 'pair? straw-pair?)
  (env-set! e 'not straw-not)
  (env-set! e 'number? straw-number?)
  (env-set! e 'string? straw-string?)
  (env-set! e 'symbol? straw-symbol?)
  (env-set! e 'boolean? straw-boolean?)
  (env-set! e 'procedure? straw-procedure?)
  (env-set! e 'set-car! straw-set-car!)
  (env-set! e 'set-cdr! straw-set-cdr!)
  (env-set! e 'make-vector straw-make-vector)
  (env-set! e 'vector-ref straw-vector-ref)
  (env-set! e 'vector-set! straw-vector-set!)
  (env-set! e 'vector-length straw-vector-length)
  (env-set! e 'vector? straw-vector?)
  (env-set! e 'display straw-display)
  (env-set! e 'newline straw-newline)
  e)
