# Lambda-calculus-compiler

## Files:
* c/main.c - c program which wraps assembly generated by compiler
* racket/asm.rkt - code parsing and intermediate representations
* racket/compiler.rkt - compiler without any optimizations
* racket/compiler-* - compiler with some optimization depending on*
* racket/lambda-calculus-utils.rkt - not part of the compiler, but contains code I used to generate programs for testing

## How to use:
First run ./compiler-the-compiler, which will compile main.c

Then, to compile a file, open compile*.rkt in drracket, and run e.g.

```
(main "../test-lang-src/prog2.jacob" "../prog2.out")
```
