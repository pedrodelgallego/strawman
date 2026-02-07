#lang racket
(require "env.rkt")
(provide (struct-out straw-class)
         (struct-out straw-instance)
         (struct-out straw-generic)
         straw-class-all-fields
         straw-class-is-a?
         straw-generic-dispatch)

;; A class: name (symbol), parent (straw-class or #f), own-fields (list of symbols)
(struct straw-class (name parent fields) #:transparent)

;; An instance: class (straw-class), field-values (vector, indexed by all-fields order)
(struct straw-instance (class field-values) #:transparent)

;; all-fields : straw-class -> (listof symbol)
;; Returns all fields including inherited ones (parent fields first).
(define (straw-class-all-fields cls)
  (if (straw-class-parent cls)
      (append (straw-class-all-fields (straw-class-parent cls))
              (straw-class-fields cls))
      (straw-class-fields cls)))

;; straw-class-is-a? : straw-class × straw-class -> boolean
;; Returns #t if cls is the same as target-cls or a subclass of it.
(define (straw-class-is-a? cls target-cls)
  (cond
    [(eq? cls target-cls) #t]
    [(straw-class-parent cls) (straw-class-is-a? (straw-class-parent cls) target-cls)]
    [else #f]))

;; A generic function: name (symbol), methods (mutable hash: straw-class -> procedure)
(struct straw-generic (name methods) #:transparent)

;; straw-generic-dispatch : straw-generic × list-of-args -> (values procedure straw-class)
;; Find the most-specific method for the first argument's class.
;; Walks up the class hierarchy (subclass first).
(define (straw-generic-dispatch gen args)
  (define first-arg (car args))
  (define methods (straw-generic-methods gen))
  (define gname (straw-generic-name gen))
  (unless (straw-instance? first-arg)
    (error (format "no method for ~a on ~a" gname first-arg)))
  (define cls (straw-instance-class first-arg))
  ;; Walk up the class hierarchy looking for a method
  (let loop ([c cls])
    (cond
      [(not c)
       (error (format "no method for ~a on ~a" gname (straw-class-name cls)))]
      [(hash-has-key? methods c)
       (hash-ref methods c)]
      [else
       (loop (straw-class-parent c))])))
