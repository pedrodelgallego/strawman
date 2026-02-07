#lang racket
(require rackunit
         "../src/eval.rkt"
         "../src/env.rkt"
         "../src/builtins.rkt"
         "../src/objects.rkt")

;; Helper: evaluate a string in the default environment
(define (eval-str str)
  (define env (default-env))
  (define exprs (read-all str))
  (define result (void))
  (for ([e (in-list exprs)])
    (set! result (straw-eval e env)))
  result)

;; Helper: evaluate multiple forms, return the environment for inspection
(define (eval-str/env str)
  (define env (default-env))
  (define exprs (read-all str))
  (for ([e (in-list exprs)])
    (straw-eval e env))
  env)

;; Helper: read all s-expressions from a string
(define (read-all str)
  (define in (open-input-string str))
  (let loop ()
    (define e (read in))
    (if (eof-object? e) '() (cons e (loop)))))

;; E10.1 — define-class

(test-case "define-class: simple class defines a class object"
  (define env (eval-str/env "(define-class Point () (x y))"))
  (define cls (env-lookup env 'Point))
  (check-true (straw-class? cls))
  (check-equal? (straw-class-name cls) 'Point)
  (check-equal? (straw-class-fields cls) '(x y))
  (check-equal? (straw-class-parent cls) #f))

(test-case "define-class: subclass inherits parent fields"
  (define env (eval-str/env
    "(define-class Point () (x y)) (define-class Point3D (Point) (z))"))
  (define cls (env-lookup env 'Point3D))
  (check-true (straw-class? cls))
  (check-equal? (straw-class-name cls) 'Point3D)
  ;; all-fields should include parent fields followed by own fields
  (check-equal? (straw-class-all-fields cls) '(x y z))
  (define parent (straw-class-parent cls))
  (check-true (straw-class? parent))
  (check-equal? (straw-class-name parent) 'Point))

(test-case "define-class: duplicate field error"
  (check-exn #rx"duplicate field"
             (lambda ()
               (eval-str "(define-class Bad () (x x))"))))

(test-case "define-class: unknown parent error"
  (check-exn #rx"unknown parent class"
             (lambda ()
               (eval-str "(define-class C (Unknown) (a))"))))

;; E10.2 — Instantiation and field access

(test-case "construct and access x"
  (check-equal?
   (eval-str "(define-class Point () (x y)) (define p (make-Point 1 2)) (Point-x p)")
   1))

(test-case "access y"
  (check-equal?
   (eval-str "(define-class Point () (x y)) (define p (make-Point 1 2)) (Point-y p)")
   2))

(test-case "mutate field"
  (check-equal?
   (eval-str "(define-class Point () (x y)) (define p (make-Point 1 2)) (set-Point-x! p 99) (Point-x p)")
   99))

(test-case "predicate yes"
  (check-equal?
   (eval-str "(define-class Point () (x y)) (define p (make-Point 1 2)) (Point? p)")
   #t))

(test-case "predicate no"
  (check-equal?
   (eval-str "(define-class Point () (x y)) (Point? 42)")
   #f))

(test-case "wrong arity"
  (check-exn #rx"arity mismatch"
             (lambda ()
               (eval-str "(define-class Point () (x y)) (make-Point 1)"))))

(test-case "wrong type on accessor"
  (check-exn #rx"expected Point"
             (lambda ()
               (eval-str "(define-class Point () (x y)) (Point-x 42)"))))

(test-case "acceptance: field sum"
  (check-equal?
   (eval-str "(define-class Point () (x y)) (define p (make-Point 3 4)) (+ (Point-x p) (Point-y p))")
   7))

;; E10.3 — Generic functions and method dispatch

(test-case "generic: single method"
  (check-equal?
   (eval-str
    (string-append
     "(define-generic area)"
     "(define-class Rect () (w h))"
     "(define-method (area (self Rect)) (* (Rect-w self) (Rect-h self)))"
     "(area (make-Rect 3 4))"))
   12))

(test-case "generic: inherited method"
  (check-equal?
   (eval-str
    (string-append
     "(define-generic area)"
     "(define-class Rect () (w h))"
     "(define-method (area (self Rect)) (* (Rect-w self) (Rect-h self)))"
     "(define-class Square (Rect) ())"
     "(area (make-Square 5 5))"))
   25))

(test-case "generic: no method"
  (check-exn #rx"no method"
             (lambda ()
               (eval-str
                (string-append
                 "(define-generic area)"
                 "(area 42)")))))

(test-case "generic: override"
  (check-equal?
   (eval-str
    (string-append
     "(define-generic area)"
     "(define-class Rect () (w h))"
     "(define-method (area (self Rect)) (* (Rect-w self) (Rect-h self)))"
     "(define-class Square (Rect) ())"
     "(define-method (area (self Square)) (* (Rect-w self) (Rect-w self)))"
     "(area (make-Square 7 7))"))
   49))

;; E10.4 — Inheritance: inherited field, own field, is-a?

(test-case "inheritance: inherited field"
  (check-equal?
   (eval-str
    (string-append
     "(define-class A () (x))"
     "(define-class B (A) (y))"
     "(A-x (make-B 1 2))"))
   1))

(test-case "inheritance: own field"
  (check-equal?
   (eval-str
    (string-append
     "(define-class A () (x))"
     "(define-class B (A) (y))"
     "(B-y (make-B 1 2))"))
   2))

(test-case "inheritance: is-a? direct"
  (check-equal?
   (eval-str
    (string-append
     "(define-class A () (x))"
     "(define-class B (A) (y))"
     "(is-a? (make-B 1 2) B)"))
   #t))

(test-case "inheritance: is-a? parent"
  (check-equal?
   (eval-str
    (string-append
     "(define-class A () (x))"
     "(define-class B (A) (y))"
     "(is-a? (make-B 1 2) A)"))
   #t))

(test-case "inheritance: is-a? unrelated"
  (check-equal?
   (eval-str
    (string-append
     "(define-class A () (x))"
     "(define-class B (A) (y))"
     "(define-class Point () (px py))"
     "(is-a? (make-B 1 2) Point)"))
   #f))
