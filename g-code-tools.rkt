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
         racket/draw
         racket/class
         parser-tools/lex
         (for-syntax racket/base)
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

  (coord-code? (code? . -> . boolean?))
  (empty-coord? (coord? . -> . boolean?))
  (x-coord? (coord? . -> . boolean?))
  (y-coord? (coord? . -> . boolean?))
  (z-coord? (coord? . -> . boolean?))
  (xy-coord? (coord? . -> . boolean?))
  (xz-coord? (coord? . -> . boolean?))
  (yz-coord? (coord? . -> . boolean?))
  (xyz-coord? (coord? . -> . boolean?))
  (i-coord? (coord? . -> . boolean?))
  (j-coord? (coord? . -> . boolean?))
  (k-coord? (coord? . -> . boolean?))
  (ij-coord? (coord? . -> . boolean?))
  (ik-coord? (coord? . -> . boolean?))
  (jk-coord? (coord? . -> . boolean?))
  (ijk-coord? (coord? . -> . boolean?))
  (coord? predicate/c)

  (param-in-command? (code? command? . -> . boolean?))
  (param-by-sym (g-code-sym? command? . -> . (or/c code? #f)))
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

  (get-coords (command?
               . -> .
               (values coord? coord?)))
  
  (update-coords (command? (coord? . -> . coord?)
                           . -> .
                           command?))

  (update-commands ((listof command?) (command?
                                       . -> .
                                       (or/c command? null (listof command?)))
                                      . -> .
                                      (listof command?)))
  
  (update-program-coords ((listof command?) (coord? . -> . coord?)
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

(define-lex-abbrev g-code-par-comment
  (re-seq "(" (re-* (char-complement #\newline)) ")"))

(define-lex-abbrev g-code-semicolon-comment
  (re-seq ";" (re-* (char-complement #\newline))))

(define-lex-abbrev g-code-word
  (re-seq g-code-sym (re-* blank) g-code-num))

(define-lex-abbrev g-code-line
  (re-+ (re-seq (re-* (re-or blank g-code-par-comment)) g-code-word)
        (re-* blank)
        g-code-semicolon-comment))

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

;; Consumes an import-port?, reads a line of G-code and produces
;; the corresponding command.
(define lex-line
  (lexer [(eof) null]
         [whitespace (lex-line input-port)]
         [g-code-par-comment
          (lex-line input-port)]
         [g-code-semicolon-comment
          (lex-line input-port)]
         [g-code-word
          (cons (lex-word (open-input-string lexeme))
                (lex-line input-port))]))

;; Consumes an input-port?, reads all the G-code and returns
;; the corresponding list of commands.
(define (read-g-code [in (current-input-port)])
  (define lex
    (lexer [(eof) null]
           [whitespace (read-g-code input-port)]
           [#\% (read-g-code input-port)]
           [g-code-par-comment
            (read-g-code input-port)]
           [g-code-line
            (cons (codes->command (lex-line (open-input-string lexeme)))
                  (read-g-code input-port))]
           [any-char
            (error 'read-g-code
                   "~a:~a Cannot read rest of line \"~a\"."
                   (position-line start-pos)
                   (+ (position-col start-pos) 1)
                   (string-append lexeme (read-line in)))]))

  (lex in))

;; -------------------- WRITING

;; Consumes a list of command? and writes to the output-port?
;; specified by out.
(define (write-g-code commands
                      [out (current-output-port)]
                      [num-decs 4])
  (define (round-num num)
    (exact->inexact (/ (round (* (inexact->exact num)
                                 (expt 10 num-decs)))
                       (expt 10 num-decs))))
  
  (define (write-code a-code)
    (display (code-sym a-code) out)
    (display (round-num (code-num a-code)) out)
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

;; Converts (cd <sym><num>) into (code '<sym> <num>).
(define-syntax (cd stx)
  (define sym+num (symbol->string (syntax->datum (cadr (syntax->list stx)))))
  (define-values (sym num) (values (string->symbol (substring sym+num 0 1))
                                   (string->number (substring sym+num 1))))
  #`(code '#,sym #,num))

;; Converts (cmd <sym><num> <p-sym><p-num> ...) into
;; (command (code '<sym> <num>) (list (code '<p-sym> <p-num>) ...))
(define-syntax (cmd stx)
  (define (codeish-stx->code-stx a-stx)
    (define sym+num (symbol->string (syntax->datum a-stx)))
    (define-values (sym num) (values (string->symbol (substring sym+num 0 1))
                                     (string->number (substring sym+num 1))))
    #`(code '#,sym #,num))
  
  (define commandish-stx (cdr (syntax->list stx)))
  (define-values (name params) (values (codeish-stx->code-stx (car commandish-stx))
                                       (map codeish-stx->code-stx (cdr commandish-stx))))
  #`(command #,name (list #,@params)))

;; Converts (cmds (<command>) ...) into
;; (list (command <name> <params>) ...)
(define-syntax (cmds stx)
  (define (codeish-stx->code-stx a-stx)
    (define sym+num (symbol->string (syntax->datum a-stx)))
    (define-values (sym num) (values (string->symbol (substring sym+num 0 1))
                                     (string->number (substring sym+num 1))))
    #`(code '#,sym #,num))

  (define (commandish-stx->command-stx a-stx)
    (define commandish-list (syntax->list a-stx))
    (define-values (name params) (values (codeish-stx->code-stx (car commandish-list))
                                         (map codeish-stx->code-stx (cdr commandish-list))))
    #`(command #,name (list #,@params)))
  
  (define programish-stx (cdr (syntax->list stx)))
  (define commands (map commandish-stx->command-stx programish-stx))
  
  #`(list #,@commands))

;; -------------------- CODE STRUCT FUNCTIONS

;; Consumes a symbol and returns a function that
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

(define (coord-code? a-code)
  (or? (x-code? a-code)
       (y-code? a-code)
       (z-code? a-code)
       (i-code? a-code)
       (j-code? a-code)
       (k-code? a-code)))

(define (empty-coord? coord)
  (= 0 (length coord)))

(define (x-coord? coord)
  (and (= 1 (length coord))
       (x-code? (list-ref coord 0))))
  
(define (y-coord? coord)
  (and (= 1 (length coord))
       (y-code? (list-ref coord 0))))

(define (z-coord? coord)
  (and (= 1 (length coord))
       (z-code? (list-ref coord 0))))

(define (xy-coord? coord)
  (and (= 2 (length coord))
       (x-code? (list-ref coord 0))
       (y-code? (list-ref coord 1))))

(define (xz-coord? coord)
  (and (= 2 (length coord))
       (x-code? (list-ref coord 0))
       (z-code? (list-ref coord 1))))

(define (yz-coord? coord)
  (and (= 2 (length coord))
       (y-code? (list-ref coord 0))
       (z-code? (list-ref coord 1))))

(define (xyz-coord? coord)
  (and (= 3 (length coord))
       (x-code? (list-ref coord 0))
       (y-code? (list-ref coord 1))
       (z-code? (list-ref coord 2))))

(define (i-coord? coord)
  (and (= 1 (length coord))
       (i-code? (list-ref coord 0))))
  
(define (j-coord? coord)
  (and (= 1 (length coord))
       (j-code? (list-ref coord 0))))

(define (k-coord? coord)
  (and (= 1 (length coord))
       (k-code? (list-ref coord 0))))

(define (ij-coord? coord)
  (and (= 2 (length coord))
       (i-code? (list-ref coord 0))
       (j-code? (list-ref coord 1))))

(define (ik-coord? coord)
  (and (= 2 (length coord))
       (i-code? (list-ref coord 0))
       (k-code? (list-ref coord 1))))

(define (jk-coord? coord)
  (and (= 2 (length coord))
       (j-code? (list-ref coord 0))
       (k-code? (list-ref coord 1))))

(define (ijk-coord? coord)
  (and (= 3 (length coord))
       (i-code? (list-ref coord 0))
       (j-code? (list-ref coord 1))
       (k-code? (list-ref coord 2))))

(define (coord? val)
  (and? (list? val)
        (andmap (lambda (a-code)
                  (and (code? a-code) (coord-code? a-code)))
                val)
        (or (empty-coord? val)
            (x-coord? val)
            (y-coord? val)
            (z-coord? val)
            (xy-coord? val)
            (xz-coord? val)
            (yz-coord? val)
            (xyz-coord? val)
            (i-coord? val)
            (j-coord? val)
            (k-coord? val)
            (ij-coord? val)
            (ik-coord? val)
            (jk-coord? val)
            (ijk-coord? val))))

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
(define (get-coords cmd)
  (define x (param-by-sym 'X cmd))
  (define y (param-by-sym 'Y cmd))
  (define z (param-by-sym 'Z cmd))
  (define i (param-by-sym 'I cmd))
  (define j (param-by-sym 'J cmd))
  (define k (param-by-sym 'K cmd))
  (define xyz-coord (filter-not false? (list x y z)))
  (define ijk-coord (filter-not false? (list i j k)))
  (values xyz-coord ijk-coord))

;; Consumes a command and an updater, and produces the same command
;; after applying the updater to the coordinates
;; of the command.
(define (update-coords cmd updater)
  (define-values (xyz-coord ijk-coord) (get-coords cmd))
  (define xyz-updated-coord (if (not (equal? #() xyz-coord))
                                (updater xyz-coord)
                                #()))
  (define ijk-updated-coord (if (not (equal? #() ijk-coord))
                                (updater ijk-coord)
                                #()))
  
  (define dummy-cmd (command (code 'G -1)
                             (append xyz-updated-coord
                                     ijk-updated-coord)))
  
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
(define (update-program-coords cmds updater)
  (map (lambda (a-cmd) (update-coords a-cmd updater))
       cmds))

;; -------------------- G-CODE DC

(define g-code-dc%
  (class* object% (dc<%>)

    (define/public (cache-font-metrics-key)
      (void))
    
    (define/public (clear)
      (void))

    (define/public (copy x y width height x2 y2)
      (void))

    (define/public (draw-arc x y width height start-radius end-radius)
      (void))

    (define/public (draw-bitmap source dest-x dest-y height x2 y2
                                [style 'solid] [color (send the-color-database find-color "black")]
                                [mask #f])
      (void))

    (define/public (draw-bitmap-section source dest-x dest-y height src-x src-y src-width src-height
                                [style 'solid] [color (send the-color-database find-color "black")]
                                [mask #f])
      (void))

    (define/public (draw-ellipse x y width height)
      (void))

    (define/public (draw-line x1 y1 x2 y2)
      (void))

    (define/public (draw-lines points [xoffset 0] [yoffset 0])
      (void))

    (define/public (draw-path path [xoffset 0] [yoffset 0] [fill-style 'odd-even])
      (void))

    (define/public (draw-point x y)
      (void))

    (define/public (draw-polygon points [xoffset 0] [yoffset 0] [fill-style 'odd-even])
      (void))

    (define/public (draw-rectangle x y width height)
      (void))

    (define/public (draw-rounded-rectangle x y width height [radius -0.25])
      (void))

    (define/public (draw-spline x1 y1 x2 y2 x3 y3)
      (void))

    (define/public (draw-text text x y [combine #f] [offset 0] [angle 0])
      (void))

    (define/public (end-doc)
      (void))

    (define/public (end-page)
      (void))

    (define/public (erase)
      (void))

    (define/public (flush)
      (void))

    (define/public (get-alpha)
      (void))

    (define/public (get-background)
      (void))

    (define/public (get-backing-scale)
      (void))

    (define/public (get-brush)
      (void))

    (define/public (get-char-height)
      (void))

    (define/public (get-char-width)
      (void))

    (define/public (get-clipping-region)
      (void))
    
    (define/public (get-device-scale)
      (void))
    
    (define/public (get-font)
      (void))
    
    (define/public (get-gl-context)
      (void))
    
    (define/public (get-initial-matrix)
      (void))

    (define/public (get-origin)
      (void))

    (define/public (get-pen)
      (void))

    (define/public (get-path-bounding-box path type)
      (void))

    (define/public (get-rotation)
      (void))

    (define/public (get-scale)
      (void))

    (define/public (get-size)
      (void))

    (define/public (get-smoothing)
      (void))

    (define/public (get-text-background)
      (void))

    (define/public (get-text-extent string [font #f] [combine? #f] [offset 0])
      (void))

    (define/public (get-text-foreground)
      (void))

    (define/public (get-text-mode)
      (void))

    (define/public (get-transformation)
      (void))

    (define/public (glyph-exists? c)
      (void))

    (define/public (ok?)
      (void))

    (define/public (resume-flush)
      (void))

    (define/public (rotate angle)
      (void))

    (define/public (scale x-scale y-scale)
      (void))

    (define/public (set-alignment-scale scale)
      (void))

    (define/public (set-alpha opacity)
      (void))

    (define/public (set-background color)
      (void))

    (public set-brush)
    (define set-brush
      (case-lambda [(brush)
                    (void)]
                   [(color style)
                    (void)]))

    (define/public (set-clipping-rect x y width height)
      (void))

    (define/public (set-clipping-region rgn)
      (void))

    (define/public (set-font font)
      (void))

    (define/public (set-initial-matrix m)
      (void))

    (define/public (set-origin x y)
      (void))

    (public set-pen)
    (define set-pen
      (case-lambda [(pen)
                    (void)]
                   [(color width style)
                    (void)]))

    (define/public (set-rotation angle)
      (void))

    (define/public (set-scale x-scale y-scale)
      (void))

    (define/public (set-smoothing mode)
      (void))

    (define/public (set-text-background color)
      (void))

    (define/public (set-text-foreground color)
      (void))

    (define/public (set-text-mode mode)
      (void))

    (define/public (set-transformation t)
      (void))

    (define/public (start-doc message)
      (void))

    (define/public (start-page)
      (void))

    (define/public (suspend-flush)
      (void))

    (define/public (transform m)
      (void))

    (define/public (translate dx dy)
      (void))

    (define/public (try-color try result)
      (void))
    
    ))