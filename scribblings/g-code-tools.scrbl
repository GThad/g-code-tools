#lang scribble/manual
@require[@for-label[g-code-tools]]

@title{G-code Tools}
@author{Gifan Thadathil}

@defmodule[g-code-tools]

G-code is a low-level numerical control language. It interfaces between
software and hardware in Computerized Numerical Control (CNC) machines.
The @racketmodname[g-code-tools] module provides a structure for G-code as well as
reading and writing functions. Furthermore, it provides a handful of functions
for manipulating G-code.

The adhoc nature of G-code is the very reason for this library. G-code is often machine
specific. Not every machine will interpret the same G-code in the same way. Not every
machine will support every defined command in G-code. Some machines don't make use
of a Cartesian coordinate space --- not to mention there are multiple competing
standards for G-code. There is no guarantee an existing G-code generator will produce
G-code that works for your machine.

This library provides tools for programatically modifying G-code. You can use
it to create programs that convert generated G-code into machine-specific G-code.

Note:

@itemize[
 @item{
  @racketmodname[g-code-tools] is intended for use with G-code generated with another program. It
  provides only limited support for manual G-coders.
 }
 @item{
  We follow the LinuxCNC standard.
 }
 ]

@section{G-code Structures}
A G-code program is made up of codes that are organized into lines. Codes
can specify commands for the machine or can act as parameters for other commands.
In any case, a proper G-code program will only have one command per line. The structures
we provide reflect this.

@defstruct[code ([letter symbol?] [number number?])]{
 Represents a single G-code code.
 
 @#reader scribble/comment-reader
 (racketblock
 ;; G0
 (code 'G 0)
 ;; X150.574
 (code 'X 150.574)
 ;; F500
 (code 'F 500)
 ;; Hello16 is okay, but it won't pass the validator!
 (code 'Hello 16)
 )
}
@defstruct[command ([name code?] [parameters (listof code?)])]{
 Represents a single G-code command.

 @#reader scribble/comment-reader
 (racketblock
 ;; G0 X100 Y100
 (command (code 'G 0) (list (code 'X 100) (code 'Y 100)))
 ;; G4 P1000
 (command (code 'G 4) (list (code 'P 1000)))
 ;; S255
 (command (code 'S 255))
 )
}
