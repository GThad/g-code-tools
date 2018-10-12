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

The ad hoc nature of G-code is the very reason for this library. G-code is often machine
specific. Not every machine will interpret the same G-code in the same way. Not every
machine will support every defined command in G-code. Some machines don't make use
of a Cartesian coordinate space --- not to mention there are multiple competing
standards for G-code. There is no guarantee an existing G-code generator will produce
G-code that works for your machine.

This library provides tools for programmatically modifying G-code. You can use
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
 ;; G0 X100 Y100 G1 X0 Y0 is okay, but it won't pass
 ;; the validator either!
 (command (code 'G 0) (list (code 'X 100) (code 'Y 100)
                            (code 'G 0) (code 'X 0) (code 'Y 0)))
 )
}

@section{Reading and Writing}

@defproc[(read-g-code (in input-port? (current-input-port)))
         (listof command?)]{
 Reads in a G-code program from the given input port. If no input port
 is given, then the current input port is used. This functions reads the
 program into a list of commands where the ith line in the program corresponds
 to the ith command in the list.

 Note that this function does not yet provide useful error messages on
 @emph{syntactically} malformed input! Furthermore, @emph{semantically}
 invalid input will be successfully read in.
}

@defproc[(write-g-code [commands (listof command?)]
                       [out output-port? (current-output-port)])
         void?]{
 Writes a list of commands as a G-code program to the given output
 port. If no output port is given, then the current output port is used.

 Each command gets written to its own line. Furthermore, this
 function does not do anything complicated; it is possible
 that the written G-code is not executable! We've noticed the following problems:

 @itemize[
 @item{Written lines can be more than 256 characters (the maximum
   defined by G-code) if the command is unusually large. This problem should never
   arise if you are using semantically valid G-code though.}
 @item{Numbers can be in the wrong form. It writes
   numbers as Racket usually does. However, Racket does not always write numbers
   correctly for G-code. For example Racket writes long decimal values in
   scientific notation (@racket[1.0234123E24]). G-code does not support
   scientific notation. Furthermore, G-code does not allow long decimals either.

   A workaround is to round all inexact numbers to a small number (2-5) of decimal
   points, depending on the accuracy of your machine. In this case, Racket will always
   write the numbers correctly.}
 ]
}

@section{Code and Command Checking}

@defproc