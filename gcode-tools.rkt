#lang racket/base
(require racket/contract
         racket/struct
         parser-tools/lex
         (prefix-in re- parser-tools/lex-sre))

;; -------------------- G-CODE STRUCTURES

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
  (command (car codes) (cdr codes)))

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
                (read-gcode input-port))]))

;; -------------------- WRITING

;; Consumes a list of command? and writes to the output-port?
;; specified by out.
(define (write-gcode commands [out (current-output-port)])
  (define (write-code a-code)
    (display (code-letter a-code) out)
    (display (code-number a-code) out)
    (display " " out))
  
  (define (write-command cmd)
    (write-code (command-name cmd))
    (map write-code (command-parameters cmd))
    (display #\newline out))
  
  (map write-command commands))