# CL-SEQMAK
Common lisp (SBCL) rewrite of SEQMAK.

`CL-SEQMAK`
===========


Introduction
------------

`CL-SEQMAK` is a rewrite of [SEQMAK](https://www.github.com/jfkimberly/SEQMAK)
using common lisp (Steel Bank Common Lisp, [SBCL](http://sbcl.org/)). `SEQMAK`
is a program for producing DNA sequences. Please refer to
[SEQMAK](https://www.github.com/jfkimberly/SEQMAK) for details.


Dependencies
------------

`CL-SEQMAK` is written in SBCL 1.4. In order to run the program you must have
SBCL installed on your computer (other versions of common lisp may work, but
have not been tested). You can download SBCL at
[http://sbcl.org/platform-table.html](http://sbcl.org/platform-table.html), or
use the package manager of your OS.

Since package dependencies in `CL-SEQMAK` are handled through
[Quicklisp](https://www.quicklisp.org/beta/), Quicklisp must also be installed.
Please refer to the Quicklisp website for installation details.


Usage
-----

To load the program, start SBCL and type the following at the REPL:

`CL-USER> (load "/path/to/cl-seqmak.lisp")`

or

`CL-USER> (load "cl-seqmak")`

if the file resides in the directory where SBCL was started. 
Once the program has been loaded, it can be run by typing

`CL-USER> (run-seqmak)`

and pressing Enter.

For further information on designing DNA sequences using `CL-SEQMAK`, read the
*Introduction* of the
[manual](https://github.com/jfkimberly/SEQMAK/blob/master/SEQMAK_MANUAL.pdf).
The manual was written for `SEQMAK`, which is the Python version of
`CL-SEQMAK`, but the user interface functionalities remain almost the same.


Citations
---------

ADD THIS!


