#lang racket

(provide tokenize
         (struct-out token))

(struct token (type value) #:transparent)

;; Read a number starting at position i, return (values num-string end-pos)
(define (read-number input i len)
  (let loop ([j i])
    (cond
      [(>= j len) (values (substring input i j) j)]
      [(let ([c (string-ref input j)])
         (or (char-numeric? c) (char=? c #\.) (and (= j i) (char=? c #\-))))
       (loop (add1 j))]
      [else (values (substring input i j) j)])))

;; Read a string starting after the opening ", return (values string-value end-pos)
(define (read-string input i len)
  (let loop ([j i] [chars '()])
    (cond
      [(>= j len)
       (error "unterminated string")]
      [(char=? (string-ref input j) #\\)
       (when (>= (add1 j) len) (error "unterminated string"))
       (define next (string-ref input (add1 j)))
       (define escaped (cond [(char=? next #\") #\"]
                             [(char=? next #\\) #\\]
                             [else (error (format "unknown escape: \\~a" next))]))
       (loop (+ j 2) (cons escaped chars))]
      [(char=? (string-ref input j) #\")
       (values (list->string (reverse chars)) (add1 j))]
      [else
       (loop (add1 j) (cons (string-ref input j) chars))])))

(define (tokenize input)
  (define len (string-length input))
  (let loop ([i 0] [tokens '()])
    (if (>= i len)
        (reverse tokens)
        (let ([ch (string-ref input i)])
          (cond
            ;; Skip whitespace
            [(char-whitespace? ch)
             (loop (add1 i) tokens)]
            ;; Comments: ; discards to end of line
            [(char=? ch #\;)
             (let skip ([j (add1 i)])
               (cond
                 [(>= j len) (loop j tokens)]
                 [(char=? (string-ref input j) #\newline) (loop (add1 j) tokens)]
                 [else (skip (add1 j))]))]
            ;; Numbers: digits, or minus followed by digit
            [(or (char-numeric? ch)
                 (and (char=? ch #\-)
                      (< (add1 i) len)
                      (char-numeric? (string-ref input (add1 i)))))
             (define-values (num-str end) (read-number input i len))
             (define num (string->number num-str))
             (loop end (cons (token 'NUMBER num) tokens))]
            ;; Strings: delimited by ", supporting \" and \\ escapes
            [(char=? ch #\")
             (define-values (str end) (read-string input (add1 i) len))
             (loop end (cons (token 'STRING str) tokens))]
            ;; Booleans: #t and #f
            [(and (char=? ch #\#)
                  (< (add1 i) len)
                  (let ([next (string-ref input (add1 i))])
                    (or (char=? next #\t) (char=? next #\f))))
             (define val (char=? (string-ref input (add1 i)) #\t))
             (loop (+ i 2) (cons (token 'BOOLEAN val) tokens))]
            ;; Parentheses
            [(char=? ch #\()
             (loop (add1 i) (cons (token 'LPAREN "(") tokens))]
            [(char=? ch #\))
             (loop (add1 i) (cons (token 'RPAREN ")") tokens))]
            ;; Symbols: any non-whitespace, non-paren sequence
            [else
             (define-values (sym-str end) (read-symbol input i len))
             (loop end (cons (token 'SYMBOL (string->symbol sym-str)) tokens))])))))

;; Read a symbol starting at position i, return (values sym-string end-pos)
(define (read-symbol input i len)
  (let loop ([j i])
    (cond
      [(>= j len) (values (substring input i j) j)]
      [(let ([c (string-ref input j)])
         (or (char-whitespace? c) (char=? c #\() (char=? c #\))))
       (values (substring input i j) j)]
      [else (loop (add1 j))])))
