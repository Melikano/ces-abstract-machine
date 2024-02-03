# Assignment 3
This includes the template files for Assignment 3.  
## Your task
- From https://pages.cpsc.ucalgary.ca/~robin/class/521/assignments/Ass-14-3.html:

```
Assignment #3:  Evaluation with the CES Machine (aka. Modern SECD Machnine:

Implement an evaluator for the extended lambda calculus!

This involves the following steps:
    - Turning (extended) lambda terms into De Bruijn notation
    - Compiling De Bruijn terms into CES machine code (see attached document i.e., ces.pdf)
    - Writing the CES machine: its  step function and running code on the machine.   (You may want a debugging mode!)
    - Writing five example programs (including some recursive ones! e.g factorial) in the extended lambda calculus.

You are expected to work with the lambda calculus extended by:
Built in Booleans: True and False together with an conditional construct (if ... then ... else ...)
Built in basic arithmetic: addition, multiplication, and comparison of integers.

This is described in more detail here: see section 3.1 on the modern SECD in particular.  These notes describe different reduction strategies, the modern SECD machine (including how to implement lists) and also the Krivine machine.
```

## AutoGrader remarks
- Do NOT change the types of any function prefixed with ``autoGrader``
- Do NOT modify files in ``Lib/*`` 

- Please only submit the files:
    - ``DeBruijn.hs``
    - ``CES.hs``
  to Gradescope.

## Directory overview
```
DeBruijn.hs
```
Contains functions for converting a lambda term to its DeBruijn form, and
erroring in the case that the lambda term has free variables.

You need to implement functions in this module.

```
CES.hs
```
Functions relating to:
  - Executing one step for the CES machine
  - Running a CES machine
  - Compiling a DeBruijn notation lambda term to CES instructions.
You need to implement the functions in this module.

```
Examples.hs
```
A file for you to write your own examples.

```
Lib/AST.hs
```
This contains the AST, and the data type for CES code.

```
Lib/ASTParse.hs
```
This contains a parser for the AST

```
A3.hs
```
This file imports everything for you, so you can write `ghci A3.hs` to play with functions in `ghci` if you would like.

```
ces.pdf
```
The original assignment document with lots of useful information.

```
Lib/Monads.hs
```
This contains many common monads that you may use.

```
Lib/RawString.hs
```
A utility using TemplateHaskell and QuasiQuotes to write raw string literals to help write example programs.

```
README.md
```
This (hopefully) helpful file.


## Hints
PLEASE READ ``ces.pdf``

# Closing remarks
Have fun!

# Bugs with template code
- Please contact the TA Jared about issues regarding the template code. He
  will be more than happy to help resolve issues..
