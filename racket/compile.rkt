#lang racket

(provide main)
(require a86)


;; For now, just returns a set program
;; will map e to the assembly when I write it

;; Exp -> a86
(define (compile e)
  (seq
   (Label 'entry)
   (Mov 'rax 10)
   (Mov 'rbx 5)
   (Add 'rax 'rbx)
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