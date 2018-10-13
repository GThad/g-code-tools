#lang racket/base

;; This module provides functions for dealing with G-code. In particular,
;; it provides:
;; 1. A representation of G-code as structures.
;; 2. Functions for parsing G-code into the internal structure.
;; 3. Functions for writing the internal structure to G-code.
;; 4. Low-level functions regarding the internal structure.
;; 5. Higher-level functions regarding the G-code program.

(require racket/contract
         racket/struct
         racket/bool
         racket/list
         racket/vector
         parser-tools/lex
         (prefix-in re- parser-tools/lex-sre))

(provide
 (contract-out
  (struct code ([letter symbol?] [number number?]))
  (struct command ([name code?] [parameters (listof code?)]))

  (read-g-code (() (input-port?) . ->* . (listof command?)))
  (write-g-code (((listof command?)) (output-port?) . ->* . void?))
  
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

  (x-coord? predicate/c)
  (y-coord? predicate/c)
  (z-coord? predicate/c)
  (xy-coord? predicate/c)
  (xz-coord? predicate/c)
  (yz-coord? predicate/c)
  (xyz-coord? predicate/c)
  (i-coord? predicate/c)
  (j-coord? predicate/c)
  (k-coord? predicate/c)
  (ij-coord? predicate/c)
  (ik-coord? predicate/c)
  (jk-coord? predicate/c)
  (ijk-coord? predicate/c)
  (coordinate? predicate/c)

  (parameter-in-command? (code? command? . -> . boolean?))
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
  (update-coordinates (command? (coordinate? . -> . coordinate?)
                       . -> . command?))

  (update-commands ((listof command?) (command? . -> . (or/c command? (listof command?)))
                    . -> . (listof command?)))
  
  (update-program-coordinates ((listof command?) (coordinate? . -> . coordinate?)
                               . -> . (listof command?)))
  ))

;; -------------------- G-CODE STRUCTURES

;; A code represents a single instruction in G-code.
(struct code (letter number)
  #:transparent
  #:extra-constructor-name make-code
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'code)
      (lambda (obj) (list (code-letter obj) (code-number obj)))))])

;; A command represents a line of G-code, which is a grouping of codes.
(struct command (name parameters)
  #:transparent
  #:extra-constructor-name make-command
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'command)
      (lambda (obj) (list (command-name obj) (command-parameters obj)))))])

;; -------------------- PARSING

(define-lex-abbrev g-code-letter
  (re-or #\G #\M #\S #\F #\R #\P #\X #\Y #\Z #\I #\J #\K
         #\g #\m #\s #\f #\r #\p #\x #\y #\z #\i #\j #\k))

(define-lex-abbrev g-code-number
  (re-seq (re-? #\- #\+)
          (re-* numeric)
          (re-? #\.)
          (re-+ numeric)))

(define-lex-abbrev g-code-comment
  (re-seq "(" (re-* (char-complement #\newline)) ")"))

(define-lex-abbrev g-code-word
  (re-seq g-code-letter (re-* blank) g-code-number))

(define-lex-abbrev g-code-line
  (re-+ (re-seq (re-* (re-or blank g-code-comment)) g-code-word)))

;; Consumes a list of code? and produces a command? out of them.
(define (codes->command codes)
  (command (car codes) (cdr codes)))

;; Consumes an import-port?, reads a G-code word and produces
;; the corresponding code?.
(define lex-word
  (lexer [(eof) null]
         [g-code-letter
          (code (string->symbol (string-upcase lexeme))
                (lex-word input-port))]
         [g-code-number (string->number lexeme)]))

;; Consumes an import-port?, reads a lin of G-code and produces
;; the corresponding command.
(define lex-line
  (lexer [(eof) null]
         [whitespace (lex-line input-port)]
         [g-code-comment
            (lex-line input-port)]
         [g-code-word
          (cons (lex-word (open-input-string lexeme))
                (lex-line input-port))]))

;; Consumes an input-port?, reads tall the G-code and returns
;; the corresponding list of commands.
(define (read-g-code [in (current-input-port)])
  (define lex
    (lexer [(eof) null]
           [whitespace (read-g-code input-port)]
           [#\% (read-g-code input-port)]
           [g-code-comment
            (read-g-code input-port)]
           [g-code-line
            (cons (codes->command (lex-line (open-input-string lexeme)))
                  (read-g-code input-port))]))

  (lex in))

;; -------------------- WRITING

;; Consumes a list of command? and writes to the output-port?
;; specified by out.
(define (write-g-code commands [out (current-output-port)])
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

;; Like or but returns #t or #f.
(define-syntax-rule (or? pred ...)
  (->boolean (or pred ...)))

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

;; Consumes a code and checks if it has the corresponding letter.
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

;; -------------------- COORDINATE FUNCTIONS

(define x-coord?
    (vector/c x-code?
              #:flat? #t))

(define y-coord?
    (vector/c y-code?
              #:flat? #t))

(define z-coord?
    (vector/c z-code?
              #:flat? #t))

(define xy-coord?
    (vector/c x-code?
              y-code?
              #:flat? #t))

(define xz-coord?
    (vector/c x-code?
              z-code?
              #:flat? #t))

(define yz-coord?
    (vector/c y-code?
              z-code?
              #:flat? #t))

(define xyz-coord?
    (vector/c x-code?
              y-code?
              z-code?
              #:flat? #t))

(define i-coord?
    (vector/c i-code?
              #:flat? #t))

(define j-coord?
    (vector/c j-code?
              #:flat? #t))

(define k-coord?
    (vector/c k-code?
              #:flat? #t))

(define ij-coord?
    (vector/c i-code?
              j-code?
              #:flat? #t))

(define ik-coord?
    (vector/c i-code?
              k-code?
              #:flat? #t))

(define jk-coord?
    (vector/c j-code?
              k-code?
              #:flat? #t))

(define ijk-coord?
    (vector/c i-code?
              j-code?
              k-code?
              #:flat? #t))

(define coordinate?
  (or/c x-coord?
        y-coord?
        z-coord?
        xy-coord?
        xz-coord?
        yz-coord?
        xyz-coord?
        i-coord?
        j-coord?
        k-coord?
        ij-coord?
        ik-coord?
        jk-coord?
        ijk-coord?))

;; -------------------- COMMAND STRUCT FUNCTIONS

;; Consumes a letter and produces a function that returns #t
;; if a command has a name with the given letter.
(define (make-letter-command? letter)
  (lambda (cmd)
    (symbol=? letter (code-letter (command-name cmd)))))

;; Consumes a command and checks if its name it has the corresponding letter.
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

;; Consumes a code? and a command? and produces #t if the code
;; is a parameter in the command.
(define (parameter-in-command? a-code cmd)
  (member? a-code (command-parameters cmd)))

;; Consumes a symbol? and a command? and produces a code
;; that matches the given letter. Otherwise it produces #f.
(define (parameter-by-letter letter cmd)
  (findf (lambda (a-code) (symbol=? letter (code-letter a-code)))
        (command-parameters cmd)))

;; Consumes a code? and a command? and produces #t if the code
;; is the name in the command.
(define (named? a-code cmd)
  (equal? a-code (command-name cmd)))

;; Consumes a command? and produces a list of coordinates. A coordinate
;; is a list with 1-3 codes depending on the number of dimensions there
;; are to the coordinate. X,Y,Z codes can be in a coordinate as well as
;; I,J,K codes.
(define (get-coordinates cmd)
  (define x (parameter-by-letter 'X cmd))
  (define y (parameter-by-letter 'Y cmd))
  (define z (parameter-by-letter 'Z cmd))
  (define i (parameter-by-letter 'I cmd))
  (define j (parameter-by-letter 'J cmd))
  (define k (parameter-by-letter 'K cmd))
  (define xyz-coord (vector-filter-not false? (vector x y z)))
  (define ijk-coord (vector-filter-not false? (vector i j k)))
  (values xyz-coord ijk-coord))

;; Consumes a command and an updater, and produces the same command
;; after applying the updater too the coordinates in the parameters
;; of the command.
(define (update-coordinates cmd updater)
  (define-values (xyz-coord ijk-coord) (get-coordinates cmd))
  (define xyz-updated-coord (if (not (equal? #() xyz-coord))
                                (updater xyz-coord)
                                #()))
  (define ijk-updated-coord (if (not (equal? #() ijk-coord))
                                (updater ijk-coord)
                                #()))
  
  (define dummy-cmd (command (code 'G -1)
                             (append (vector->list xyz-updated-coord)
                                     (vector->list ijk-updated-coord))))
  
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