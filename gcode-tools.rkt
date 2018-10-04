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

;; -------------------- CODES->COMMANDS

;; Consumes two codes, and checks if param-code
;; can be used as a parameter for a command with
;; name name-code.
;; Note: This function really defines our supported codes.
;; Adding rudimentary support for a new code entails adding
;; a new function and corresponding match entry.
(define (parameter-for-command? param-code name-code)

  (define (parameter-for-G0?)
    (member? (code-letter param-code)
             '(X Y Z)))

  (define (parameter-for-G1?)
    (member? (code-letter param-code)
             '(X Y Z F)))

  (define (parameter-for-G2-G3?)
    (member? (code-letter param-code)
             '(X Y Z I J K F R)))
  
  (match name-code
    [(code 'G 0) (parameter-for-G0?)]
    [(code 'G 1) (parameter-for-G1?)]
    [(code 'G 2) (parameter-for-G2-G3?)]
    [(code 'G 3) (parameter-for-G2-G3?)]
    [_ #f]))


;; Consumes a list of code? and produce a list of command? that groups
;; the codes into commands.
;;
;; Note: This function does not guarantee a command list the user
;; wants since there are multiple sensible command lists for the
;; same code list. This function tries to be canonical.
(define (codes->commands codes)
  
  ;; The helper function builds the command list. cmds+stack is a list (cmds new-cmd):
  
  ;; cmds holds the list of commands to be returned.
  ;; new-cmd holds each partially built command as a list of code?.

  ;; helper looks at next-code and cons it to
  ;; new-cmd if its part of the command, otherwise
  ;; it adds new-cmd to cmds, and starts
  ;; a new command with next-code. If next-code is #f,
  ;; then we know we are done, and can return cmds.

  ;; Note cmds and new-cmd are reversed, so we can use cons rather than append.
  (define (helper next-code cmds+new-cmd)
    (define-values (cmds new-cmd) (values (first cmds+new-cmd) (second cmds+new-cmd)))
    
    ;; Produces a command? from new-cmd.
    (define (new-cmd->command)
      (define rev-cmd (reverse new-cmd))
      (command (first rev-cmd) (rest rev-cmd)))

    ;; Produces #t whenever next-code is part of new-cmd. This happens
    ;; when
    ;; 1. next-code is a proper parameter of the partially built new-cmd.
    ;; 2. A parameter with the same letter as next-code is not already
    ;;    defined in new-cmd.
    (define (part-of-new-cmd?)
      (and? (parameter-for-command? next-code (last new-cmd))
            (not (findf (lambda (a-code)
                          (code-letter=? a-code next-code))
                        new-cmd))))
    
    (cond [(equal? #f next-code) (cons (new-cmd->command) cmds)]
          [(part-of-new-cmd?)
           (list cmds
                 (cons next-code new-cmd))]
           
          [else
           (list (cons (new-cmd->command) cmds)
                 (list next-code))]))
  
  (reverse (foldl helper
                  (list null (list (first codes)))
                  (append (rest codes) '(#f)))))