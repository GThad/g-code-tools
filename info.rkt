#lang info
(define collection "g-code-tools")
(define deps '("base" "parser-tools-lib"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/g-code-tools.scrbl" ())))
(define pkg-desc "A collection of tools for manipulating G-code.")
(define version "0.1")
(define pkg-authors '(gifan))
