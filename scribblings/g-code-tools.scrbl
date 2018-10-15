#lang scribble/manual
@require[@for-label[g-code-tools
                    racket/base
                    racket/contract
                    racket/list]]

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

@itemlist[
 @item{
  @racketmodname[g-code-tools] is intended for use with G-code generated with another program.
  It provides only limited support for manual G-coders.
 }
 @item{
  We follow the @hyperlink["http://www.linuxcnc.org/docs/html/gcode/g-code.html"]{LinuxCNC}
  standard for parsing and validation, but ensure contracts place looser restrictions.
 }
 ]

@section{G-code Structures}
A G-code program consists of commands organized into lines. A proper
program has a single command per line. A command is made up of codes.
A single code can be a command or multiple codes can together define
a command. The structures we provide reflect the organization of G-code.

@defproc[(g-code-sym? [val any/c]) boolean?]{
 Consumes anything and produces @racket[#t] if @racket[val] is any of
 @racket['G], @racket['M], @racket['F], @racket['S], @racket['P], @racket['R],
 @racket['X], @racket['Y], @racket['Z], @racket['I], @racket['J], or @racket['K].
 Produces @racket[#f] otherwise.
}

@defstruct[code ([sym g-code-sym?] [num number?])]{
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

@defstruct[command ([name code?] [params (listof code?)])]{
 Represents a single G-code command.

 @#reader scribble/comment-reader
 (racketblock
 ;; G0 X100 Y100
 (command (code 'G 0) (list (code 'X 100) (code 'Y 100)))
 ;; G4 P1000
 (command (code 'G 4) (list (code 'P 1000)))
 ;; S255
 (command (code 'S 255))
 ;; G0 X100 Y100 G1 X0 Y0 is okay, but it is invalid G-code since
 ;; there are 2 actual commands.
 (command (code 'G 0) (list (code 'X 100) (code 'Y 100)
 (code 'G 0) (code 'X 0) (code 'Y 0)))
 )
}

@section{Reading and Writing}

@defproc[(read-g-code (in input-port? (current-input-port)))
         (listof command?)]{
 Reads in a G-code program from @racket[in]. If @racket[in] is not specified,
 then the current input port is used. Produces a list of commands
 where the ith line of the program corresponds
 to the ith command in the list.

 Note that we do not yet provide useful error messages on
 @emph{syntactically} malformed input! Furthermore, @emph{semantically}
 invalid input will be successfully read in.
}

@defproc[(write-g-code [cmds (listof command?)]
                       [out output-port? (current-output-port)])
         void?]{
 Writes a list of commands as a G-code program to @racket[out].
 If @racket[out] is not specified, then the current output port is used.

 As for style, each command is written to a new line, and that is all.
 No comments or anything else is added to minimize file sizes.
 In addition, it is possible that the written G-code is syntactically malformed! We've
 noticed the following problems:

 @itemlist[
 @item{Written lines can be more than 256 characters (the maximum
   defined by G-code) if the command is unusually large. This problem should rarely
   arise if you are using semantically valid G-code, and keep numbers relatively small.}
 @item{Numbers can be in the wrong form. Numbers are written as Racket usually writes them.
   However, Racket does not always write numbers
   correctly for G-code. For example, Racket writes long decimal values in
   scientific notation (@racket[1.0234123E24]). G-code does not support
   scientific notation.

   A workaround is to round all inexact numbers to a small number (2-5) of decimal
   points, depending on the accuracy of your machine. In this case, Racket should
   write the numbers correctly.}
 #:style 'ordered]

 These issues will be fixed in the future.
}

@section{Code and Command Functions}

@defproc*[#:kind "procedures"
          ([(g-code? [cd code?]) boolean?]
           [(m-code? [cd code?]) boolean?]
           [(f-code? [cd code?]) boolean?]
           [(s-code? [cd code?]) boolean?]
           [(r-code? [cd code?]) boolean?]
           [(p-code? [cd code?]) boolean?]
           [(x-code? [cd code?]) boolean?]
           [(y-code? [cd code?]) boolean?]
           [(z-code? [cd code?]) boolean?]
           [(i-code? [cd code?]) boolean?]
           [(j-code? [cd code?]) boolean?]
           [(k-code? [cd code?]) boolean?])]{
 Consumes a code and produces @racket[#t] if @racket[(code-sym cd)] matches the
 expected symbol. Produces @racket[#f] otherwise.
}

@defproc*[#:kind "procedures"
          ([(g-command? [cmd command?]) boolean?]
           [(m-command? [cmd command?]) boolean?]
           [(f-command? [cmd command?]) boolean?]
           [(s-command? [cmd command?]) boolean?]
           [(r-command? [cmd command?]) boolean?]
           [(p-command? [cmd command?]) boolean?]
           [(x-command? [cmd command?]) boolean?]
           [(y-command? [cmd command?]) boolean?]
           [(z-command? [cmd command?]) boolean?]
           [(i-command? [cmd command?]) boolean?]
           [(j-command? [cmd command?]) boolean?]
           [(k-command? [cmd command?]) boolean?])]{
 Consumes a command and produces @racket[#t] if @racket[(code-sym (command-name cmd))]
 matches the expected symbol. Produces @racket[#f] otherwise.
}

@defproc[(named? [cd code?] [cmd command?])
         (or/c code? #f)]{
 Consumes a code and a command. Produces @racket[#t] if @racket[(command-name cmd)]
 equals @racket[cd]. Produces @racket[#f] otherwise.
}

@defproc[(param-in-command? [cd code?] [cmd command?])
         boolean?]{
 Consumes a code and a command, and produces @racket[#t] if @racket[cd] is a member
 of @racket[(command-params cmd)]. Produces @racket[#f] otherwise.
}

@defproc[(param-by-sym [sym g-code-sym?] [cmd command?])
         (or/c code? #f)]{
 Consumes a symbol and a command. If @racket[(command-params cmd)] has a member
 @racket[cd] such that @racket[(code-sym cd)] matches @racket[sym], then @racket[cd]
 is produced. Produces @racket[#f] otherwise.
}

@section{Coordinates}
Some commands operate on coordinates, which are specified with a certain group of
codes. For example "G0 X20 Y20 Z20" tells the machine to move quickly to coordinate
(20, 20, 20). The X, Y, and Z codes together specify the coordinate. Within Racket, a coordinate
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
 Consumes anything and produces @racket[#t] if @racket[vec] is in the correct form:
 @itemlist[
 @item{Only codes should be in @racket[vec].}
 @item{The number of codes should match the expected number of dimensions.}
 @item{The order of codes should be canonical. For example X,Y,Z is canonical, but Y,X,Z is not.}
 #:style 'ordered]

 Examples may explain this more clearly.

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

@defproc[(coord? [val any/c]) boolean?]{
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

@defproc[(get-coords [cmd command?]) (values coord? coord?)]{
 Consumes a command and returns two coordinates. The first coordinate contains
 any X, Y, and Z codes in @racket[(command-params cmd)]. The second coordinate contains any
 I, J, and K codes in @racket[(command-params cmd)]. If a command does not contain
 a code, the code will not be included in the resulting vector.
}

@defproc[(update-coords [cmd command?] [updater (-> coord? coord?)])
         command?]{
 Consumes a command and an updater function. Produces the same command with updated
 coordinate codes. The coordinate codes are gathered with
 @racket[(get-coords cmd)], and @racket[updater] is applied to each coordinate.
 The codes in the resulting coordinates replace the old ones in @racket[cmd].
}

@section{Program Functions}

@defproc[(update-commands [cmds (listof command?)]
                          [updater (-> command? (or/c null command? (listof command?)))])
         (listof command?)]{
 Equivalent to @racket[(flatten (map updater cmds))]. By using @racket[flatten], we
 easily add functionality over a typical map. If @racket[updater] produces
 @racket[null], we do a remove command operation. If it produces a command, we do a replace
 command operation. If it produces a list of commands, we do a replace command with many commands
 operation.
}

@defproc[(update-program-coords [cmds (listof command?)]
                                [updater (-> coord? coord?)])
         (listof command?)]{
 Equivalent to
 @racketblock[(map (lambda (a-cmd) (update-coords a-cmd updater)) cmds)]
}

@section{Possible New Features}
The following lists (in no particular order) new functionality/changes that are planned.
We make no guarantees, but we will try. Anyone is free to send a pull request!

@itemlist[
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

@itemlist[
 @item{@bold{0.1 --- 2018-10-15}
  @itemlist[@item{First release.}]
 }
 ]