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
  (g-code-sym? (any/c . -> . boolean?))
  
  (struct code ([sym g-code-sym?] [num number?]))
  (struct command ([name code?] [params (listof code?)]))

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

  (empty-coord? (vector? . -> . boolean?))
  (x-coord? (vector? . -> . boolean?))
  (y-coord? (vector? . -> . boolean?))
  (z-coord? (vector? . -> . boolean?))
  (xy-coord? (vector? . -> . boolean?))
  (xz-coord? (vector? . -> . boolean?))
  (yz-coord? (vector? . -> . boolean?))
  (xyz-coord? (vector? . -> . boolean?))
  (i-coord? (vector? . -> . boolean?))
  (j-coord? (vector? . -> . boolean?))
  (k-coord? (vector? . -> . boolean?))
  (ij-coord? (vector? . -> . boolean?))
  (ik-coord? (vector? . -> . boolean?))
  (jk-coord? (vector? . -> . boolean?))
  (ijk-coord? (vector? . -> . boolean?))
  (coordinate? predicate/c)

  (param-in-command? (code? command? . -> . boolean?))
  (param-by-sym (g-code-sym? command? . -> . boolean?))
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

  (get-coordinates (command?
                    . -> .
                    (values coordinate? coordinate?)))
  
  (update-coordinates (command? (coordinate? . -> . coordinate?)
                       . -> .
                       command?))

  (update-commands ((listof command?) (command?
                                       . -> .
                                       (or/c command? null (listof command?)))
                    . -> .
                    (listof command?)))
  
  (update-program-coordinates ((listof command?) (coordinate? . -> . coordinate?)
                               . -> .
                               (listof command?)))
  ))

;; -------------------- G-CODE STRUCTURES

;; Consumes anything and returns whether it is
;; a G-code symbol.
(define g-code-sym?
  (one-of/c 'G 'M 'F 'S 'R 'P
            'X 'Y 'Z 'I 'J 'K))

;; A code represents a single instruction in G-code.
(struct code (sym num)
  #:transparent
  #:extra-constructor-name make-code
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'code)
      (lambda (obj) (list (code-sym obj) (code-num obj)))))])

;; A command represents a line of G-code, which is a grouping of codes.
(struct command (name params)
  #:transparent
  #:extra-constructor-name make-command
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) 'command)
      (lambda (obj) (list (command-name obj) (command-params obj)))))])

;; -------------------- PARSING

(define-lex-abbrev g-code-sym
  (re-or #\G #\M #\S #\F #\R #\P #\X #\Y #\Z #\I #\J #\K
         #\g #\m #\s #\f #\r #\p #\x #\y #\z #\i #\j #\k))

(define-lex-abbrev g-code-num
  (re-seq (re-? #\- #\+)
          (re-* numeric)
          (re-? #\.)
          (re-+ numeric)))

(define-lex-abbrev g-code-comment
  (re-seq "(" (re-* (char-complement #\newline)) ")"))

(define-lex-abbrev g-code-word
  (re-seq g-code-sym (re-* blank) g-code-num))

(define-lex-abbrev g-code-line
  (re-+ (re-seq (re-* (re-or blank g-code-comment)) g-code-word)))

;; Consumes a list of code? and produces a command? out of them.
(define (codes->command codes)
  (command (car codes) (cdr codes)))

;; Consumes an import-port?, reads a G-code word and produces
;; the corresponding code?.
(define lex-word
  (lexer [(eof) null]
         [g-code-sym
          (code (string->symbol (string-upcase lexeme))
                (lex-word input-port))]
         [g-code-num (string->number lexeme)]))

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
    (display (code-sym a-code) out)
    (display (code-num a-code) out)
    (display " " out))
  
  (define (write-command cmd)
    (write-code (command-name cmd))
    (map write-code (command-params cmd))
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
;; consumes a code? and produces true whenever the symbol
;; of the code matches symbol.
(define (make-sym-code? sym)  
  (lambda (a-code)
    (symbol=? sym (code-sym a-code))))

;; Consumes a code and checks if it has the corresponding sym.
(define g-code? (make-sym-code? 'G))
(define m-code? (make-sym-code? 'M))
(define f-code? (make-sym-code? 'F))
(define s-code? (make-sym-code? 'S))
(define x-code? (make-sym-code? 'X))
(define y-code? (make-sym-code? 'Y))
(define z-code? (make-sym-code? 'Z))
(define i-code? (make-sym-code? 'I))
(define j-code? (make-sym-code? 'J))
(define k-code? (make-sym-code? 'K))
(define r-code? (make-sym-code? 'R))
(define p-code? (make-sym-code? 'P))

;; -------------------- COORDINATE FUNCTIONS

(define empty-coord?
  (vector/c #:flat? #t))

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
  (or/c empty-coord?
        x-coord?
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

;; Consumes a symol and produces a function that returns #t
;; if a command has a name with the given symbol.
(define (make-sym-command? sym)
  (lambda (cmd)
    (symbol=? sym (code-sym (command-name cmd)))))

(define g-command? (make-sym-command? 'G))
(define m-command? (make-sym-command? 'M))
(define f-command? (make-sym-command? 'F))
(define s-command? (make-sym-command? 'S))
(define x-command? (make-sym-command? 'X))
(define y-command? (make-sym-command? 'Y))
(define z-command? (make-sym-command? 'Z))
(define i-command? (make-sym-command? 'I))
(define j-command? (make-sym-command? 'J))
(define k-command? (make-sym-command? 'K))
(define p-command? (make-sym-command? 'P))
(define r-command? (make-sym-command? 'R))

;; Consumes a code? and a command? and produces #t if the code
;; is a parameter in the command.
(define (param-in-command? a-code cmd)
  (member? a-code (command-params cmd)))

;; Consumes a symbol? and a command? and produces a code
;; that matches the given symbol. Otherwise it produces #f.
(define (param-by-sym sym cmd)
  (findf (lambda (a-code) (symbol=? sym (code-sym a-code)))
        (command-params cmd)))

;; Consumes a code? and a command? and produces #t if the code
;; is the name in the command.
(define (named? a-code cmd)
  (equal? a-code (command-name cmd)))

;; Consumes a command? and produces a list of coordinates.
(define (get-coordinates cmd)
  (define x (param-by-sym 'X cmd))
  (define y (param-by-sym 'Y cmd))
  (define z (param-by-sym 'Z cmd))
  (define i (param-by-sym 'I cmd))
  (define j (param-by-sym 'J cmd))
  (define k (param-by-sym 'K cmd))
  (define xyz-coord (vector-filter-not false? (vector x y z)))
  (define ijk-coord (vector-filter-not false? (vector i j k)))
  (values xyz-coord ijk-coord))

;; Consumes a command and an updater, and produces the same command
;; after applying the updater to the coordinates
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
    (define new-param/false (param-by-sym (code-sym param)
                                                 dummy-cmd))
    (if new-param/false
        new-param/false
        param))
  
  (command (command-name cmd)
           (map keep/replace (command-params cmd))))

;; -------------------- PROGRAM FUNCTIONS
;; Consumes a list of commands and an update function,
;; and produces a the same list of commands after applying the
;; updater function onto each command.
(define (update-commands cmds updater)
  (flatten (map updater cmds)))

;; Consumes a list of commands and an update function.
;; Produces the same commands after applying the updater
;; to coordinates of each command.
(define (update-program-coordinates cmds updater)
  (map (lambda (a-cmd) (update-coordinates a-cmd updater))
       cmds))