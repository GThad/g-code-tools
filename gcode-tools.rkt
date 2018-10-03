#lang racket/base
(require racket/string)

;; gcode-atom represents an atomic instruction in G-code. For example,
;; "G0" and "X25.5" would correspond to (gcode-atom G 0) and (gcode-atom X 25.5)
;;
;; letter: symbol?
;; number: number?
(struct gcode-atom (letter number))

;; gcode-command represents a command in G-code. For example,
;; "G0 X25.5 Y30" would correspond to
;; (gcode-command (gcode-atom G 0) '((gcode-atom X 25.5) (gcode-atom Y 30)))
;;
;; command: gcode-atom?
;; arguments: (listof gcode-atom?)
(struct gcode-command (command arguments))

;; gcode->atoms: str -> (listof gcode-atoms?)
;; This function consumes a string of G-code and produces a list of
;; corresponding gcode-atom? corresponding.
;; Note this function places no restrictions on the form of str other than
;; it be proper G-code. This function does not care about whitespace.
(define (gcode->atoms str)
  (define (remove-white-space a-str)
    (string-replace a-str #px"\\s" ""))
  (define (remove-comments a-str)
    (string-replace a-str #px"\\(.*\\)" ""))
  (define (split-into-atoms a-str)
    (regexp-match* #px"([G|M|S|F|X|Y|Z|I|J|K])([-|+]?\\d*\\.?\\d+)"
                  a-str
                  #:match-select cdr))
  (define (pair->atom a-pair)
    (gcode-atom (string->symbol (car a-pair))
                (string->number (cadr a-pair))))
  
  (map pair->atom
       (split-into-atoms (remove-comments (remove-white-space str)))))

;; atom->commands: (listof gcode-atom?) -> (listof gcode-command?)
;; This function consumes a list of gcode-atom? and produces a list of
;; corresponding gcode-command?. It does this by looking at each atom. If
;; we see a command atom, then we grab atoms until the next
;; command atom.
(define (atoms>commands atom-list)
  ...)