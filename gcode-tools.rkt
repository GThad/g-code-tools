#lang racket/base
(require racket/string
         racket/contract
         racket/bool
         racket/function
         racket/match
         racket/list
         racket/struct
         parser-tools/lex
         (prefix-in re- parser-tools/lex-sre))

;; -------------------- UTILITY FUNCTIONS/MACROS

;; Coerce everything except #f to a boolean.
(define-syntax-rule (->boolean val)
  (if val #t #f))

;; Like and but returns #t or #f.
(define-syntax-rule (and? pred ...)
  (->boolean (and pred ...)))

;; Like member but returns #t or #f.
(define-syntax-rule (member? val lst)
  (->boolean (member val lst)))

;; -------------------- INTERNAL STRUCTURE 

;; A code represents a single instruction in G-code. For example,
;; "G0" and "X25.5" would correspond to (code G 0) and (code X 25.5)
;;
;; letter: symbol?
;; number: number?
(struct code (letter number)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'code)
      (lambda (obj) (list (code-letter obj) (code-number obj)))))])

;; A command represents a group of instructions in G-code corresponding to some single action.
;; For example, "G0 X25.5 Y30" would correspond to
;; (command (code G 0) '((code X 25.5) (code Y 30)))
;;
;; name: code?
;; parameters (listof code?)
(struct command (name parameters)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'command)
      (lambda (obj) (list (command-name obj) (command-parameters obj)))))])

;; -------------------- CODE FUNCTIONS

;; Consumes a symbol and returns a function that
;; consumes a code? and produces true whenever the letter
;; of the code matches letter.
(define (make-letter-code-checker letter)
  (lambda (a-code)
    (symbol=? letter (code-letter a-code))))

(define (g-code? a-code)
  (or ((make-letter-code-checker 'G) a-code)
      ((make-letter-code-checker 'g) a-code)))

(define (m-code? a-code)
  (or ((make-letter-code-checker 'M) a-code)
      ((make-letter-code-checker 'm) a-code)))

;; -------------------- PARSING

(define-lex-abbrev gcode-letter
  (re-or #\G #\M #\S #\F #\X #\Y #\Z #\I #\J #\K
         #\g #\m #\s #\f #\x #\y #\z #\i #\j #\k))

(define-lex-abbrev gcode-number
  (re-seq (re-? #\- #\+)
          (re-* numeric)
          (re-? #\.)
          (re-+ numeric)))

;; Consume a string that looks like a code? and product the corresponding code?
(define (code-str->code str)
  (define split-str
    (cdr (regexp-match #px"([G|M|S|F|X|Y|Z|I|J|K])([-|+]?\\d*\\.?\\d+)"
                   (string-replace str #px"\\s" ""))))
  (code (string->symbol (first split-str))
        (string->number (second split-str))))

;; Consumes an input-port?, reads from the input-port exactly
;; enough characters to produce a single token.
(define import-gcode
  (lexer [(eof) null]
         ;; Recursive call effectively skips whitespace
         [whitespace (import-gcode input-port)]
         [(re-seq gcode-letter (re-* whitespace) gcode-number)
          (cons (code-str->code lexeme) (import-gcode input-port))]))