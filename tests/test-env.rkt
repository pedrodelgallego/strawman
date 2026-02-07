#lang racket

(require rackunit
         "../src/env.rkt")


;; E1.3 — Environment: Variable bindings with lexical scope

(test-case "set and lookup: set x→1, lookup x returns 1"
  (define e (make-env))
  (env-set! e 'x 1)
  (check-equal? (env-lookup e 'x) 1))

(test-case "unbound lookup: lookup y in empty env raises error"
  (define e (make-env))
  (check-exn #rx"unbound variable: y"
             (lambda () (env-lookup e 'y))))

(test-case "parent lookup: child finds x in parent"
  (define parent (make-env))
  (env-set! parent 'x 1)
  (define child (make-env parent))
  (check-equal? (env-lookup child 'x) 1))

(test-case "parent chain lookup: grandchild finds x in grandparent"
  (define grandparent (make-env))
  (env-set! grandparent 'x 42)
  (define parent (make-env grandparent))
  (define child (make-env parent))
  (check-equal? (env-lookup child 'x) 42))

(test-case "shadowing: child x→2 shadows parent x→1"
  (define parent (make-env))
  (env-set! parent 'x 1)
  (define child (make-env parent))
  (env-set! child 'x 2)
  (check-equal? (env-lookup child 'x) 2)
  (check-equal? (env-lookup parent 'x) 1))

(test-case "update existing: set x→1, update x→2, lookup x returns 2"
  (define e (make-env))
  (env-set! e 'x 1)
  (env-update! e 'x 2)
  (check-equal? (env-lookup e 'x) 2))

(test-case "update unbound: update y in empty env raises error"
  (define e (make-env))
  (check-exn #rx"cannot set! unbound variable: y"
             (lambda () (env-update! e 'y 99))))

(test-case "extend with params/args: extend env with (a b) → (1 2), lookup a→1 b→2"
  (define parent (make-env))
  (env-set! parent 'x 99)
  (define child (env-extend parent '(a b) (list 1 2)))
  (check-equal? (env-lookup child 'a) 1)
  (check-equal? (env-lookup child 'b) 2)
  ;; child can still see parent bindings
  (check-equal? (env-lookup child 'x) 99))

(test-case "parent not mutated by child: extend, set x in child, parent x → unbound"
  (define parent (make-env))
  (define child (env-extend parent '() '()))
  (env-set! child 'x 42)
  (check-equal? (env-lookup child 'x) 42)
  (check-exn #rx"unbound variable: x"
             (lambda () (env-lookup parent 'x))))

(test-case "extend arity mismatch: (a b) with (1) raises error"
  (define parent (make-env))
  (check-exn #rx"arity mismatch"
             (lambda () (env-extend parent '(a b) (list 1)))))
