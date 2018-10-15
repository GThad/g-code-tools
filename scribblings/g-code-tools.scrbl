#lang scribble/manual
@require[@for-label[g-code-tools racket/base racket/contract]]

@title{G-code Tools}
@author{Gifan Thadathil}

@defmodule[g-code-tools]

G-code is a low-level numerical control language. It interfaces between
software and hardware in
@hyperlink["https://en.wikipedia.org/wiki/Numerical_control"]{Computerized Numerical Control}
(CNC) machines.
The @racketmodname[g-code-tools] module provides a structure for G-code as well as
reading and writing functions. Furthermore, it provides a handful of functions
for manipulating G-code.

The ad hoc nature of G-code motivates this library's existence. G-code is often machine
specific. Not every machine will interpret commands in the same way, and not every
machine will mechanically support every defined command in G-code. One of the author's machines
didn't use the Cartesian coordinate system G-code assumes --- not to mention there are multiple
competing standards for G-code. Since many CNC machines make use of automatically generated
G-code, it is likely the generated G-code will not work for your machine.

This package provides tools for programmatically modifying G-code. You can use
it to create programs that convert generated G-code into machine-specific G-code.

Note:

@itemize[
 @item{
  @racketmodname[g-code-tools] is intended for use with G-code generated with another program.
  It provides only limited support for manual G-coders.
 }
 @item{
  We follow the @hyperlink["http://www.linuxcnc.org/docs/html/gcode/g-code.html"]{LinuxCNC}
  standard.
 }
 ]

@section{G-code Structures}
A G-code program consists of codes organized into lines. Codes
specify commands for a machine or act as parameters for other commands.
In any case, a proper G-code program will only have one command per line. The structures
we provide reflect this.

@defproc[(g-code-letter? [val any/c]) boolean?]{
 Consumes anything and produces @racket[#t] if @racket[val] is any of
 @racket['G], @racket['M], @racket['F], @racket['S], @racket['P], @racket['R],
 @racket['X], @racket['Y], @racket['Z], @racket['I], @racket['J], or @racket['K]. Otherwise
 produces @racket[#f].
}

@defstruct[code ([letter g-code-letter?] [number number?])]{
 Represents a single G-code code.
 
 @#reader scribble/comment-reader
 (racketblock
 ;; G0
 (code 'G 0)
 ;; X150.574
 (code 'X 150.574)
 ;; F500
 (code 'F 500)
 ;; Throws exn:fail
 (code 'Hello 16)
 ;; Throws exn:fail
 (code 'g 16)
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
 ;; G0 X100 Y100 G1 X0 Y0 is okay, but it is invalid G-code.
 (command (code 'G 0) (list (code 'X 100) (code 'Y 100)
                            (code 'G 0) (code 'X 0) (code 'Y 0)))
 )
}

@section{Reading and Writing}

@defproc[(read-g-code (in input-port? (current-input-port)))
         (listof command?)]{
 Reads in a G-code program from the given input port. If no input port
 is given, then the current input port is used. This functions reads the
 program into a list of commands where the ith line of the program corresponds
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

 Each command is written to a new line. Also this
 function does not do anything complicated; it is possible
 that the written G-code is not executable! We've noticed the following problems:

 @itemize[
 @item{Written lines can be more than 256 characters (the maximum
   defined by G-code) if the command is unusually large. This problem should never
   arise if you are using semantically valid G-code though.}
 @item{Numbers can be in the wrong form. It writes
   numbers as Racket usually does. However, Racket does not always write numbers
   correctly for G-code. For example, Racket writes long decimal values in
   scientific notation (@racket[1.0234123E24]). G-code does not support
   scientific notation.

   A workaround is to round all inexact numbers to a small number (2-5) of decimal
   points, depending on the accuracy of your machine. In this case, Racket will always
   write the numbers correctly.}
 ]
}

@section{Code and Command Functions}

@defproc*[#:kind "procedures"
          ([(g-code? [a-code code?]) boolean?]
           [(m-code? [a-code code?]) boolean?]
           [(f-code? [a-code code?]) boolean?]
           [(s-code? [a-code code?]) boolean?]
           [(r-code? [a-code code?]) boolean?]
           [(p-code? [a-code code?]) boolean?]
           [(x-code? [a-code code?]) boolean?]
           [(y-code? [a-code code?]) boolean?]
           [(z-code? [a-code code?]) boolean?]
           [(i-code? [a-code code?]) boolean?]
           [(j-code? [a-code code?]) boolean?]
           [(k-code? [a-code code?]) boolean?])]{
 Consumes a code and produces @racket[#t] if the code has the corresponding
 letter in the letter component. Otherwise, it produces @racket[#f].
}

@defproc*[#:kind "procedures"
          ([(g-command? [a-command command?]) boolean?]
           [(m-command? [a-command command?]) boolean?]
           [(f-command? [a-command command?]) boolean?]
           [(s-command? [a-command command?]) boolean?]
           [(r-command? [a-command command?]) boolean?]
           [(p-command? [a-command command?]) boolean?]
           [(x-command? [a-command command?]) boolean?]
           [(y-command? [a-command command?]) boolean?]
           [(z-command? [a-command command?]) boolean?]
           [(i-command? [a-command command?]) boolean?]
           [(j-command? [a-command command?]) boolean?]
           [(k-command? [a-command command?]) boolean?])]{
 Consumes a command and produces @racket[#t] if the command has a code with
 the corresponding letter in the name component of the command. Otherwise, it produces
 @racket[#f].
}

@defproc[(named? [a-code code?] [a-command command?])
         (or/c code? #f)]{
 Consumes a code and a command, and produces @racket[#t] if the command's name is
 equal to the given code. Otherwise, it produces @racket[#f].
}

@defproc[(parameter-in-command? [a-code code?] [a-command command?])
         boolean?]{
 Consumes a code and a command, and produces @racket[#t] if the code is part of the
 command's parameters. Otherwise, it produces @racket[#f].
}

@defproc[(parameter-by-letter [letter symbol?] [a-command command?])
         (or/c code? #f)]{
 Consumes a symbol and a command. If the given command has a parameter with the
 given letter, then it produces that code object. Otherwise, it produces @racket[#f].
}

@section{Coordinates}
Some commands operate on coordinates, which are specified with a certain group of
codes. For example "G0 X20 Y20 Z20" tells the machine to move quickly to coordinate
(20, 20, 20). The X, Y, and Z codes specify the coordinate here. Within Racket, a coordinate
is a @racket[vector] with one, two, or three elements
depending on the number of dimensions. Each element should be an X, Y, Z, I, J, or K code.

@defproc*[#:kind "procedures"
          ([(empty-coord? [vec vector?]) boolean?]
           [(x-coord? [vec vector?]) boolean?]
           [(y-coord? [vec vector?]) boolean?]
           [(z-coord? [vec vector?]) boolean?]
           [(xy-coord? [vec vector?]) boolean?]
           [(xz-coord? [vec vector?]) boolean?]
           [(xyz-coord? [vec vector?]) boolean?]
           [(i-coord? [vec vector?]) boolean?]
           [(j-coord? [vec vector?]) boolean?]
           [(k-coord? [vec vector?]) boolean?]
           [(ij-coord? [vec vector?]) boolean?]
           [(ik-coord? [vec vector?]) boolean?]
           [(ijk-coord? [vec vector?]) boolean?])]{
 Consumes anything and produces @racket[#t] if the given argument is a vector
 in the correct form. The number of codes in the vector should match the number
 of expected dimensions, and the order of the codes should be canonical. The following
 examples better explain this.

 @racketinput[(x-coord? #((code 'X 20)))]
 @racketresultblock[#t]
 @racketinput[(x-coord? #((code 'Y 20)))]
 @racketresultblock[#f]
 @racketinput[(x-coord? #((code 'X 20) (code 'Y 20)))]
 @racketresultblock[#f]
 
 @racketinput[(xy-coord? #((code 'X 20) (code 'Y 20)))]
 @racketresultblock[#t]
 @racketinput[(xy-coord? #((code 'X 20)))]
 @racketresultblock[#f]
 @racketinput[(xy-coord? #((code 'Y 20) (code 'X 20)))]
 @racketresultblock[#f]
}

@defproc[(coordinate? [val any]) boolean?]{
 Equivalent to
 @racketblock[
 (or (empty-coord? val)
     (x-coord? val)
     (y-coord? val)
     (z-coord? val)
     (xy-coord? val)
     (xz-coord? val)
     (xyz-coord? val)
     (i-coord? val)
     (j-coord? val)
     (k-coord? val)
     (ij-coord? val)
     (ik-coord? val)
     (ijk-coord? val))
 ]
 where the result is converted to a boolean.
}

@defproc[(get-coordinates [command command?]) (values coordinate? coordinate?)]{
 Consumes a command and returns two coordinates. The first coordinate contains
 any X, Y, and Z codes in the given command. The second coordinate contains any
 I, J, and K codes in the given command. If a command does not contain the code, it will
 not be included in the resulting vector.
}

@defproc[(update-coordinates [command command?] [updater (-> coordinate? coordinate?)])
         command?]{
 Consumes a command and an updater function. Produces the same command with updated
 coordinate codes. The coordinate codes are gathered with
 @racket[(get-coordinates command)], and the updater is applied to each coordinate.
 The codes in the resulting coordinate replace the old ones in the original command.
}

@section{Program Functions}

@defproc[(update-commands [commands (listof command?)]
                          [updater (-> command? (or/c command? null (listof command?)))])
         (listof command?)]{
 Equivalent to @racket[(flatten (map updater commands))]. By using @racket[flatten], we can
 easily add functionality over a typical map. If the updater produces
 @racket[null], then the command given to it will be removed. If the updater produces
 a command, then the given command will be replaced the new one. If the updater
 produces a list of commands, then the given command will be replaced with all the
 commands in the list.
}

@defproc[(update-program-coordinates [commands (listof command?)]
                                     [updater (-> coordinate? coordinate?)])
         (listof command?)]{
 Equivalent to
 @racketblock[(map (lambda (a-cmd) (update-coordinates a-cmd updater)) commands)]
}

@section{Possible New Features}
The following lists (in no particular order) new functionality/changes that are planned. We do not guarantee
any of them will be done, but we will try. Anyone is free to send a pull request!

@itemize[
 @item{Stronger restrictions on contracts.}
 @item{Proper parsing errors.}
 @item{Ensuring writing always produces valid G-code.}
 @item{Better support for more codes. For example, a function for changing all F codes, which
 represent feed rates.}
 @item{Support for semi-colon comments appearing at the end of lines.}
 @item{Semantic validation of G-code. This means ensuring multiple commands aren't on the same
 line, checking for proper parameters to commands, etc.}
 @item{Tool-path simulations.}
 @item{Sorting functions for minimizing non-work movement.}
 @item{G-code to pict and pict to G-code conversion. Other converters?}
 @item{Multi-threaded functions.}
 ]

@section{Changelog}

@itemize[
 @item{@bold{0.1 --- 2018-10-15}
  @itemize[@item{First release.}]
 }
 ]