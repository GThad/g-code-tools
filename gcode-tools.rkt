#lang racket/base

;; This module provides functions for dealing with G-code. In particular,
;; it provides:
;; 1. A representation of G-code as structures.
;; 2. Functions for parsing G-code into the internal structure.
;; 3. Functions for writing the internal structure to G-code.
;; 4. Low-level regarding the internal structure.
;; 5. Higher-level functions regarding the G-code program.

(require racket/contract
         racket/struct
         racket/bool
         racket/list
         parser-tools/lex
         (prefix-in re- parser-tools/lex-sre))

(provide
 (contract-out
  (struct code ([letter symbol?] [number number?]))
  (struct command ([name code?] [parameters (listof code?)]))
  (struct coordinate ([dim-1 (or/c code? #f)]
                      [dim-2 (or/c code? #f)]
                      [dim-3 (or/c code? #f)]))

  (read-gcode (() (input-port?) . ->* . (listof command?)))
  (write-gcode (((listof command?)) (output-port?) . ->* . void?))
  
  (g-code? (code? . -> . boolean?))
  (m-code? (code? . -> . boolean?))
  (f-code? (code? . -> . boolean?))
  (s-code? (code? . -> . boolean?))
  (r-code? (code? . -> . boolean?))
  (p-code? (code? . -> . boolean?))
  (x-code? (code? . -> . boolean?))
  (y-code? (code? . -> . boolean?))
  (z-code? (code? . -> . boolean?))
  (i-code? (code? . -> . boolean?))
  (j-code? (code? . -> . boolean?))
  (k-code? (code? . -> . boolean?))
  (code=? (() #:rest code? . ->* . boolean?))

  (x-coord? (coordinate? . -> . boolean?))
  (y-coord? (coordinate? . -> . boolean?))
  (z-coord? (coordinate? . -> . boolean?))
  (xy-coord? (coordinate? . -> . boolean?))
  (xz-coord? (coordinate? . -> . boolean?))
  (yz-coord? (coordinate? . -> . boolean?))
  (xyz-coord? (coordinate? . -> . boolean?))
  (i-coord? (coordinate? . -> . boolean?))
  (j-coord? (coordinate? . -> . boolean?))
  (k-coord? (coordinate? . -> . boolean?))
  (ij-coord? (coordinate? . -> . boolean?))
  (ik-coord? (coordinate? . -> . boolean?))
  (jk-coord? (coordinate? . -> . boolean?))
  (ijk-coord? (coordinate? . -> . boolean?))

  (parameter? (code? command? . -> . boolean?))
  (parameter-by-letter (symbol? command? . -> . boolean?))
  (named? (code? command? . -> . boolean?))

  (g-command? (command? . -> . boolean?))
  (m-command? (command? . -> . boolean?))
  (f-command? (command? . -> . boolean?))
  (s-command? (command? . -> . boolean?))
  (r-command? (command? . -> . boolean?))
  (p-command? (command? . -> . boolean?))
  (x-command? (command? . -> . boolean?))
  (y-command? (command? . -> . boolean?))
  (z-command? (command? . -> . boolean?))
  (i-command? (command? . -> . boolean?))
  (j-command? (command? . -> . boolean?))
  (k-command? (command? . -> . boolean?))

  (get-coordinates (command? . -> . (values coordinate? coordinate?)))
  (update-coordinates (command? (coordinate? . -> . coordinate?) . -> . command?))

  (update-commands ((listof command?) (command? . -> . (or/c command? (listof command?))) . -> . (listof command?)))
  (update-program-coordinates ((listof command?) (coordinate? . -> . coordinate?) . -> . (listof command?)))
  ))

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
  (re-or #\G #\M #\S #\F #\R #\P #\X #\Y #\Z #\I #\J #\K
         #\g #\m #\s #\f #\r #\p #\x #\y #\z #\i #\j #\k))

(define-lex-abbrev gcode-number
  (re-seq (re-? #\- #\+)
          (re-* numeric)
          (re-? #\.)
          (re-+ numeric)))

(define-lex-abbrev gcode-comment
  (re-seq "(" (re-* (char-complement #\newline)) ")"))

(define-lex-abbrev gcode-word
  (re-seq gcode-letter (re-* blank) gcode-number))

(define-lex-abbrev gcode-line
  (re-+ (re-* (re-or blank gcode-comment)) gcode-word (re-or blank gcode-comment)))

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
         [gcode-comment
            (lex-line input-port)]
         [gcode-word
          (cons (lex-word (open-input-string lexeme))
                (lex-line input-port))]))

;; Consumes an input-port? and produces a list of command? for the
;; G-code.
(define (read-gcode [in (current-input-port)])
  (define lex
    (lexer [(eof) null]
           [whitespace (read-gcode input-port)]
           [#\% (read-gcode input-port)]
           [gcode-comment
            (read-gcode input-port)]
           [gcode-line
            (cons (codes->command (lex-line (open-input-string lexeme)))
                  (read-gcode input-port))]))

  (lex in))

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
  
  (map write-command commands)
  (void))

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
(define r-code? (make-letter-code? 'R))
(define p-code? (make-letter-code? 'P))

;; Consumes 2 or more code? and returns #t when they are all
;; equal?
(define (code=? . codes)
  (cond [(> (length codes) 1)
         (and (equal? (first codes) (second codes))
              (apply code=? (rest codes)))]
        [else #t]))

;; -------------------- COORDINATE FUNCTIONS

;; Consumes a coordinate? and produces #t if the coordinate only
;; has an x dimension defined.
(define (x-coord? coord)
  (and? (coordinate-dim-1 coord)
        (x-code? (coordinate-dim-1 coord))
        (not (coordinate-dim-2 coord))
        (not (coordinate-dim-3 coord))))

(define (y-coord? coord)
  (and? (coordinate-dim-2 coord)
        (y-code? (coordinate-dim-2 coord))
        (not (coordinate-dim-1 coord))
        (not (coordinate-dim-3 coord))))

(define (z-coord? coord)
  (and? (coordinate-dim-3 coord)
        (z-code? (coordinate-dim-3 coord))
        (not (coordinate-dim-1 coord))
        (not (coordinate-dim-2 coord))))

(define (xy-coord? coord)
  (and? (coordinate-dim-1 coord)
        (coordinate-dim-2 coord)
        (x-code? (coordinate-dim-1 coord))
        (y-code? (coordinate-dim-2 coord))
        (not (coordinate-dim-3 coord))))

(define (xz-coord? coord)
  (and? (coordinate-dim-1 coord)
        (coordinate-dim-3 coord)
        (x-code? (coordinate-dim-1 coord))
        (z-code? (coordinate-dim-3 coord))
        (not (coordinate-dim-2 coord))))

(define (yz-coord? coord)
  (and? (coordinate-dim-2 coord)
        (coordinate-dim-3 coord)
        (y-code? (coordinate-dim-2 coord))
        (z-code? (coordinate-dim-3 coord))
        (not (coordinate-dim-1 coord))))

(define (xyz-coord? coord)
  (and? (coordinate-dim-1 coord)
        (coordinate-dim-2 coord)
        (coordinate-dim-3 coord)
        (x-code? (coordinate-dim-1 coord))
        (y-code? (coordinate-dim-2 coord))
        (z-code? (coordinate-dim-3 coord))))

(define (i-coord? coord)
  (and? (coordinate-dim-1 coord)
        (i-code? (coordinate-dim-1 coord))
        (not (coordinate-dim-2 coord))
        (not (coordinate-dim-3 coord))))

(define (j-coord? coord)
  (and? (coordinate-dim-2 coord)
        (j-code? (coordinate-dim-2 coord))
        (not (coordinate-dim-1 coord))
        (not (coordinate-dim-3 coord))))

(define (k-coord? coord)
  (and? (coordinate-dim-3 coord)
        (k-code? (coordinate-dim-3 coord))
        (not (coordinate-dim-1 coord))
        (not (coordinate-dim-2 coord))))

(define (ij-coord? coord)
  (and? (coordinate-dim-1 coord)
        (coordinate-dim-2 coord)
        (i-code? (coordinate-dim-1 coord))
        (j-code? (coordinate-dim-2 coord))
        (not (coordinate-dim-3 coord))))

(define (ik-coord? coord)
  (and? (coordinate-dim-1 coord)
        (coordinate-dim-3 coord)
        (i-code? (coordinate-dim-1 coord))
        (k-code? (coordinate-dim-3 coord))
        (not (coordinate-dim-2 coord))))

(define (jk-coord? coord)
  (and? (coordinate-dim-2 coord)
        (coordinate-dim-3 coord)
        (j-code? (coordinate-dim-2 coord))
        (k-code? (coordinate-dim-3 coord))
        (not (coordinate-dim-1 coord))))

(define (ijk-coord? coord)
  (and? (coordinate-dim-1 coord)
        (coordinate-dim-2 coord)
        (coordinate-dim-3 coord)
        (i-code? (coordinate-dim-1 coord))
        (j-code? (coordinate-dim-2 coord))
        (k-code? (coordinate-dim-3 coord))))

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
(define p-command? (make-letter-command? 'P))
(define r-command? (make-letter-command? 'R))

;; Consumes a command? and produces a list of coordinates. A coordinate
;; is a list with 1-3 codes depending on the number of dimensions there
;; are to the coordinate. X,Y,Z codes can be in a coordinate as well as
;; I,J,K codes.
(define (get-coordinates cmd)
  (define xyz-coord (coordinate (parameter-by-letter 'X cmd)
                                (parameter-by-letter 'Y cmd)
                                (parameter-by-letter 'Z cmd)))
  (define ijk-coord (coordinate (parameter-by-letter 'I cmd)
                                (parameter-by-letter 'J cmd)
                                (parameter-by-letter 'K cmd)))
  (values xyz-coord ijk-coord))

;; Consumes a command and an updater, and produces the same command
;; after applying the updater too the coordinates in the parameters
;; of the command.
(define (update-coordinates cmd updater)
  (define-values (xyz-coord ijk-coord) (get-coordinates cmd))
  (define xyz-updated-coord (updater xyz-coord))
  (define ijk-updated-coord (updater ijk-coord))
  
  (define dummy-cmd (command (code 'G -1)
                             (filter-not false?
                                         (list (coordinate-dim-1 xyz-updated-coord)
                                               (coordinate-dim-2 xyz-updated-coord)
                                               (coordinate-dim-3 xyz-updated-coord)
                                               (coordinate-dim-1 ijk-updated-coord)
                                               (coordinate-dim-2 ijk-updated-coord)
                                               (coordinate-dim-3 ijk-updated-coord)))))
  
    (define (keep/replace param)
    (define new-param/false (parameter-by-letter (code-letter param)
                                                 dummy-cmd))
    (if new-param/false
        new-param/false
        param))
  
  (command (command-name cmd)
           (map keep/replace (command-parameters cmd))))

;; -------------------- PROGRAM FUNCTIONS
;; Consumes a list of commands and an update function,
;; and produces a the same list of commands after applying the
;; updater function onto each command.
(define (update-commands cmds updater)
  (flatten (map updater cmds)))

(define (update-program-coordinates cmds updater)
  (map (lambda (a-cmd) (update-coordinates a-cmd updater))
       cmds))