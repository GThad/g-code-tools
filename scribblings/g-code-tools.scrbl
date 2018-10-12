#lang scribble/manual
@require[@for-label[g-code-tools]]

@title{g-code-tools}
@author{Gifan Thadathil}

@defmodule[g-code-tools]

G-code is a low-level numerical control language. It interfaces between
software and hardware in Computerized Numerical Control (CNC) machines.
The @racket[g-code-tools] module provides a structure for G-code as well as
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
  @racket[g-code-tools] is intended for use with G-code generated with another program. It
  provides only limited support for manual G-coders.
 }
 @item{
  We follow the LinuxCNC standard.
 }
 ]

@section{G-code Structures}
A G-code program is made up of codes (G0, X150, etc.) that are organized into lines. Words
can specify commands for the machine or can act as parameters for other commands.
In any case, a proper G-code program will only have one command per line. The structures
we provide reflect this.

@defstruct[code ([letter symbol?] [number number?])]
