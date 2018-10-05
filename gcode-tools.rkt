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

;; Consumes 2 or more code? and returns #t when they are all
;; equal?
(define (code=? . codes)
  (cond [(> (length codes) 1)
         (and (equal? (first codes) (second codes))
              (apply code=? (rest codes)))]
        [else #t]))

;; Consumes 2 or more code? and returns #t when they are all
;; have equal? letter.
(define (code-letter=? . codes)
  (cond [(> (length codes) 1)
         (and (symbol=? (code-letter (first codes)) (code-letter (second codes)))
              (apply code-letter=? (rest codes)))]
        [else #t]))

;; -------------------- PARSING

(define-lex-abbrev gcode-letter
  (re-or #\G #\M #\S #\F #\X #\Y #\Z #\I #\J #\K
         #\g #\m #\s #\f #\x #\y #\z #\i #\j #\k))

(define-lex-abbrev gcode-number
  (re-seq (re-? #\- #\+)
          (re-* numeric)
          (re-? #\.)
          (re-+ numeric)))

(define-lex-abbrev gcode-word
  (re-seq gcode-letter (re-* blank) gcode-number))

(define-lex-abbrev gcode-line
  (re-+ (re-* blank) gcode-word (re-* blank)))

;; Consumes a list of code? and produces a command? out of them.
(define (codes->command codes)
  (command (first codes) (rest codes)))

;; Consumes an import-port? and produces a code for the
;; G-code word.
(define lex-word
  (lexer [(eof) null]
         [gcode-letter
          (code (string->symbol lexeme)
                (lex-word input-port))]
         [gcode-number (string->number lexeme)]))

;; Consumes an import-port? and produces a list of code for the
;; G-code line.
(define lex-line
  (lexer [(eof) null]
         [whitespace (lex-line input-port)]
         [gcode-word
          (cons (lex-word (open-input-string lexeme))
                (lex-line input-port))]))

;; Consumes an input-port? and produces a list of command? for the
;; G-code.
(define read-gcode
  (lexer [(eof) null]
         [whitespace (read-gcode input-port)]
         [gcode-line
          (cons (codes->command (lex-line (open-input-string lexeme)))
                (import-gcode input-port))]))
