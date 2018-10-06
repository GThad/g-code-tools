#lang racket/base
(require racket/contract
         racket/struct
         racket/bool
         racket/list
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

;; A coordinate represents a 3-dimensional coordinate. It is made up of three
;; codes. The first code is either X,I the second code is either Y,J
;; and the third code is either Z,K.
;; Any of the dimensions can also be #f which means the actual dimension is unknown.
;; In this manner 1 and 2 dimensional coordinates are also supported.
(struct coordinate (dim-1 dim-2 dim-3)
  #:transparent)

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
          (code (string->symbol (string-upcase lexeme))
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

;; -------------------- CODE STRUCT FUNCTIONS

;; Consumes a string and returns a function that
;; consumes a code? and produces true whenever the letter
;; of the code matches letter.
(define (make-letter-code? letter)  
  (lambda (a-code)
    (symbol=? letter (code-letter a-code))))

(define g-code? (make-letter-code? 'G))
(define m-code? (make-letter-code? 'M))
(define f-code? (make-letter-code? 'F))
(define s-code? (make-letter-code? 'S))
(define x-code? (make-letter-code? 'X))
(define y-code? (make-letter-code? 'Y))
(define z-code? (make-letter-code? 'Z))
(define i-code? (make-letter-code? 'I))
(define j-code? (make-letter-code? 'J))
(define k-code? (make-letter-code? 'K))

;; Consumes 2 or more code? and returns #t when they are all
;; equal?
(define (code=? . codes)
  (cond [(> (length codes) 1)
         (and (equal? (first codes) (second codes))
              (apply code=? (rest codes)))]
        [else #t]))

;; -------------------- COMMAND STRUCT FUNCTIONS

;; Consumes a code? and a command? and produces #t if the code
;; is a parameter in the command.
(define (parameter? a-code cmd)
  (member? a-code (command-parameters cmd)))

;; Consumes a symbol and a command? and produces a code
;; that matches the given letter. Otherwise it produces #f.
(define (parameter-by-letter letter cmd)
  (findf (lambda (a-code) (symbol=? letter (code-letter a-code)))
        (command-parameters cmd)))

;; Consumes a code? and a command? and produces #t if the code
;; is the name in the command.
(define (named? a-code cmd)
  (code=? a-code (command-name cmd)))

;; Consumes a letter and produces a function that returns #t
;; if a command has a name with the given letter.
(define (make-letter-command? letter)
  (lambda (cmd)
    (symbol=? letter (code-letter (command-name cmd)))))

(define g-command? (make-letter-command? 'G))
(define m-command? (make-letter-command? 'M))
(define f-command? (make-letter-command? 'F))
(define s-command? (make-letter-command? 'S))
(define x-command? (make-letter-command? 'X))
(define y-command? (make-letter-command? 'Y))
(define z-command? (make-letter-command? 'Z))
(define i-command? (make-letter-command? 'I))
(define j-command? (make-letter-command? 'J))
(define k-command? (make-letter-command? 'K))

;; Update the name of a single command
(define (update-name cmd updater)
  (command (updater (command-name cmd))
           (command-parameters cmd)))

;; Update the parameters of a single command
(define (update-parameters cmd updater)
  (command (command-name cmd)
           (updater (command-parameters cmd))))

