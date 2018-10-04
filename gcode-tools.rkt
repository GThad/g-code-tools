#lang racket/base
(require racket/string
         racket/contract
         racket/bool
         racket/function
         racket/match
         racket/list
         racket/struct
         parser-tools/lex
         parser-tools/yacc)

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

;; -------------------- TOKENS

;; Defines tokens used during lexing. Produces
;; functions for creating token structs.
;;     (token-CODE-LETTER value) -> (token 'CODE-LETTER value)
;;     (token-CODE-NUMBER value) -> (token 'CODE-NUMBER value)
;; where value is anything.
(define-tokens code-tokens (CODE-LETTER CODE-NUMBER))
(define-empty-tokens empty-tokens (EOF))

;; Consumes anything and produces #t whenever expr is a
;; CODE-LETTER token.
(define (token-CODE-LETTER? expr)
  (and (token? expr)
       (symbol=? 'CODE-LETTER (token-name expr))))

;; Consumes anything and produces #t whenever expr is a
;; CODE-NUMBER token.
(define (token-CODE-NUMBER? expr)
  (and (token? expr)
       (symbol=? 'CODE-NUMBER (token-name expr))))

;; Consumes anything and produces #t whenever the argument is
;; a CODE-LETTER or CODE-NUMBER token.
(define gcode-token?
  (or/c token-CODE-LETTER?
        token-CODE-NUMBER?))

;;-------------------- LEXING

;; Consumes an input-port?, reads from the input-port exactly
;; enough characters to produce a single token.
(define gcode-input->token
  (lexer [(eof) (token-EOF)]
         ;; Recursive call effectively skips whitespace
         [whitespace (gcode-input->token input-port)]
         [(union #\G #\M #\S #\F #\X #\Y #\Z #\I #\J #\K
                 #\g #\m #\s #\f #\x #\y #\z #\i #\j #\k)
          (token-CODE-LETTER lexeme)]
         [numeric
          (token-CODE-NUMBER (string->number lexeme))]))

;;-------------------- PARSING

;; Consumes a generator that produces tokens, and parses them into
;; code?
(define tokens->internal-gcode
    (parser
     (tokens code-tokens empty-tokens)
     (start code-list)
     (end EOF)
     (error (lambda (tok-ok? tok-name tok-value)
              (print "Error")))
     (grammar (code-list
               ((code) (list $1))
               ((code code-list) (cons $1 $2)))
              (code
               ((CODE-LETTER CODE-NUMBER) (code $1 $2))))))

;; High-level parsing function consuming an input-port? with G-Code and producing
;; an equivalent list of code?.
(define (import-gcode input)
  (tokens->internal-gcode (lambda () (gcode-input->token input))))