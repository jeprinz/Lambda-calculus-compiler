#lang racket

(provide main)
(require a86)
(require "ast.rkt")


;; Exp -> a86
(define (compileImpl e)
  (match e
    [(list 'const n)
     (Push n)]
    [(list 'plus e1 e2)
     (seq
      (compileImpl e1)
      (compileImpl e2)
      (Pop 'rax)
      (Pop 'rbx)
      (Add 'rax 'rbx)
      (Push 'rax))]))


;; Exp -> a86
(define (compile e)
  (seq
   (Label 'entry)
   (compileImpl (desugar e))
   (Pop 'rax)
   (Ret)))


;;inputs filename, outputs s-expression contained in file
(define (read-file filename)
  (define in (open-input-file filename))
  (define res (read in))
  (close-input-port in)
  res)

(define (main filename outfilename)
  (define input (read-file filename))
  (define a86asm (compile input))
  (define nasmAsm (asm-string a86asm))
  (define out (open-output-file "tmp.tmp"))
  (write-string nasmAsm out)
  (close-output-port out)
  (system "nasm -f elf64 -o tmp.tmp.o tmp.tmp")
  (system "rm tmp.tmp")
  (system (string-append "gcc ../build/main.o ./tmp.tmp.o -o " outfilename))
  (system "rm tmp.tmp.o"))

(define (compile-test filename)
   (define input (read-file filename))
  (define a86asm (compile input))
  a86asm)