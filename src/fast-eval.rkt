#lang racket
(require "env.rkt"
         "pretreat.rkt")
(provide fast-eval (struct-out fast-closure))

(struct fast-closure (params body env ribs) #:transparent)

;; Exception struct for throw: carries a tag and a value.
(struct exn:straw-throw (tag value) #:transparent)

;; Exception struct for return-from: carries a block name and a value.
(struct exn:straw-return-from (name value) #:transparent)

;; rib-lookup : (listof vector) × nat × nat -> value
;; Look up a value in the rib chain by depth and offset.
(define (rib-lookup ribs depth offset)
  (vector-ref (list-ref ribs depth) offset))

;; fast-eval : treated-expr × env [× continuation] -> value
(define (fast-eval expr env [k values])
  (with-handlers
    ([exn:straw-throw?
      (lambda (t)
        (error (format "no matching catch for tag: ~a" (exn:straw-throw-tag t))))]
     [exn:straw-return-from?
      (lambda (r)
        (error (format "unknown block name: ~a" (exn:straw-return-from-name r))))])
    (fast-eval/k expr env '() k)))

;; eval-body : (listof treated-expr) × env × ribs × continuation -> value
(define (eval-body exprs env ribs k)
  (cond
    [(null? exprs) (k (void))]
    [(null? (cdr exprs)) (fast-eval/k (car exprs) env ribs k)]
    [else (fast-eval/k (car exprs) env ribs
                       (lambda (_) (eval-body (cdr exprs) env ribs k)))]))

;; eval-list : (listof treated-expr) × env × ribs × ((listof value) -> value) -> value
(define (eval-list exprs env ribs k)
  (if (null? exprs)
      (k '())
      (fast-eval/k (car exprs) env ribs
                   (lambda (v)
                     (eval-list (cdr exprs) env ribs
                                (lambda (vs) (k (cons v vs))))))))

;; fast-eval/k : treated-expr × env × ribs × continuation -> value
;; Dispatch on treated-expr struct type instead of pattern-matching on lists.
(define (fast-eval/k expr env ribs k)
  (cond
    [(treated-const? expr)
     (k (treated-const-value expr))]

    [(treated-lexical-ref? expr)
     (k (rib-lookup ribs (treated-lexical-ref-depth expr) (treated-lexical-ref-offset expr)))]

    [(treated-ref? expr)
     (k (env-lookup env (treated-ref-name expr)))]

    [(treated-quote? expr)
     (k (treated-quote-datum expr))]

    [(treated-if? expr)
     (fast-eval/k (treated-if-test expr) env ribs
                  (lambda (tv)
                    (if (not (eq? tv #f))
                        (fast-eval/k (treated-if-consequent expr) env ribs k)
                        (fast-eval/k (treated-if-alternative expr) env ribs k))))]

    [(treated-define? expr)
     (fast-eval/k (treated-define-value-expr expr) env ribs
                  (lambda (v)
                    (env-set! env (treated-define-name expr) v)
                    (k (void))))]

    [(treated-define-fun? expr)
     (env-set! env (treated-define-fun-name expr)
               (fast-closure (treated-define-fun-params expr)
                             (treated-define-fun-body expr)
                             env
                             ribs))
     (k (void))]

    [(treated-set!? expr)
     (fast-eval/k (treated-set!-value-expr expr) env ribs
                  (lambda (v)
                    (env-update! env (treated-set!-name expr) v)
                    (k (void))))]

    [(treated-begin? expr)
     (eval-body (treated-begin-exprs expr) env ribs k)]

    [(treated-lambda? expr)
     (k (fast-closure (treated-lambda-params expr)
                      (treated-lambda-body expr)
                      env
                      ribs))]

    [(treated-let? expr)
     (define bindings (treated-let-bindings expr))
     (define names (map car bindings))
     (define init-exprs (map cdr bindings))
     (eval-list init-exprs env ribs
                (lambda (vals)
                  (define new-ribs (cons (list->vector vals) ribs))
                  (define let-env (env-extend env names vals))
                  (eval-body (treated-let-body expr) let-env new-ribs k)))]

    [(treated-let*? expr)
     (define bindings (treated-let*-bindings expr))
     (let eval-let*-bindings ([bs bindings] [e env] [rs ribs])
       (if (null? bs)
           (eval-body (treated-let*-body expr) e rs k)
           (fast-eval/k (cdr (car bs)) e rs
                        (lambda (v)
                          (eval-let*-bindings
                           (cdr bs)
                           (env-extend e (list (car (car bs))) (list v))
                           (cons (vector v) rs))))))]

    [(treated-letrec? expr)
     (define bindings (treated-letrec-bindings expr))
     (define names (map car bindings))
     (define letrec-env (env-extend env names (make-list (length names) (void))))
     (define rib-vec (make-vector (length names) (void)))
     (define new-ribs (cons rib-vec ribs))
     (let eval-letrec-bindings ([bs bindings] [i 0])
       (if (null? bs)
           (eval-body (treated-letrec-body expr) letrec-env new-ribs k)
           (fast-eval/k (cdr (car bs)) letrec-env new-ribs
                        (lambda (v)
                          (env-set! letrec-env (car (car bs)) v)
                          (vector-set! rib-vec i v)
                          (eval-letrec-bindings (cdr bs) (+ i 1))))))]

    [(treated-and? expr)
     (let loop ([es (treated-and-exprs expr)])
       (cond
         [(null? es) (k #t)]
         [(null? (cdr es)) (fast-eval/k (car es) env ribs k)]
         [else
          (fast-eval/k (car es) env ribs
                       (lambda (val)
                         (if (eq? val #f) (k #f) (loop (cdr es)))))]))]

    [(treated-or? expr)
     (let loop ([es (treated-or-exprs expr)])
       (cond
         [(null? es) (k #f)]
         [(null? (cdr es)) (fast-eval/k (car es) env ribs k)]
         [else
          (fast-eval/k (car es) env ribs
                       (lambda (val)
                         (if (not (eq? val #f)) (k val) (loop (cdr es)))))]))]

    [(treated-catch? expr)
     (fast-eval/k (treated-catch-tag-expr expr) env ribs
                  (lambda (tag)
                    (with-handlers
                      ([exn:straw-throw?
                        (lambda (t)
                          (if (equal? (exn:straw-throw-tag t) tag)
                              (k (exn:straw-throw-value t))
                              (raise t)))])
                      (fast-eval/k (treated-catch-body-expr expr) env ribs k))))]

    [(treated-throw? expr)
     (fast-eval/k (treated-throw-tag-expr expr) env ribs
                  (lambda (tag)
                    (fast-eval/k (treated-throw-value-expr expr) env ribs
                                 (lambda (val)
                                   (raise (exn:straw-throw tag val))))))]

    [(treated-call/cc? expr)
     (fast-eval/k (treated-call/cc-proc-expr expr) env ribs
                  (lambda (proc)
                    (cond
                      [(or (fast-closure? proc) (procedure? proc))
                       (call-with-current-continuation
                        (lambda (racket-k)
                          (define k-proc
                            (lambda (v)
                              (racket-k (k v))))
                          (if (fast-closure? proc)
                              (let* ([call-ribs (cons (vector k-proc) (fast-closure-ribs proc))]
                                     [call-env (env-extend (fast-closure-env proc)
                                                           (fast-closure-params proc)
                                                           (list k-proc))])
                                (eval-body (fast-closure-body proc) call-env call-ribs k))
                              (k (proc k-proc)))))]
                      [else (error "call/cc: expected procedure")])))]

    [(treated-block? expr)
     (with-handlers
       ([exn:straw-return-from?
         (lambda (r)
           (if (eq? (exn:straw-return-from-name r) (treated-block-name expr))
               (k (exn:straw-return-from-value r))
               (raise r)))])
       (eval-body (treated-block-body expr) env ribs k))]

    [(treated-unwind-protect? expr)
     (with-handlers
       ([(lambda (_) #t)
         (lambda (exn)
           (eval-body (treated-unwind-protect-cleanup-exprs expr) env ribs
                      (lambda (_) (raise exn))))])
       (fast-eval/k (treated-unwind-protect-body-expr expr) env ribs
                    (lambda (v)
                      (eval-body (treated-unwind-protect-cleanup-exprs expr) env ribs
                                 (lambda (_) (k v))))))]

    [(treated-return-from? expr)
     (fast-eval/k (treated-return-from-value-expr expr) env ribs
                  (lambda (v)
                    (raise (exn:straw-return-from (treated-return-from-name expr) v))))]

    [(treated-app? expr)
     (fast-eval/k (treated-app-func-expr expr) env ribs
                  (lambda (func)
                    (eval-list (treated-app-arg-exprs expr) env ribs
                               (lambda (args)
                                 (cond
                                   [(fast-closure? func)
                                    (define call-ribs (cons (list->vector args) (fast-closure-ribs func)))
                                    (define call-env (env-extend (fast-closure-env func)
                                                                 (fast-closure-params func)
                                                                 args))
                                    (eval-body (fast-closure-body func) call-env call-ribs k)]
                                   [(procedure? func) (k (apply func args))]
                                   [else (error (format "not a procedure: ~a" func))])))))]

    [else (error (format "fast-eval: unknown treated expression: ~a" expr))]))
