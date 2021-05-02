#lang racket

(require a86)
(require "constants.rkt")

(define (instruction inst)
  (match inst
    [(Call 'rax)
     (list #xff #xd3)]
    [(Ret)
     (list #xc3)]
    ))

;; Need to deal with variables. The one variable (one arg functions)
;; ((Variable) struct from constants.rkt)
;; will be translated to a subroutine that copies whatever is pointed at by
;; rax (which holds the argument) to rbx location.
;; All things that are written should be null terminated therefore.

;; a86 -> bytes
;; (the bytes are just a list of numbers because we will input them to a86)
(define (assemble code)
  (apply append (map instruction code)))

;; a86 -> a86
;; Takes assembly, and produces a bunch of mov instructions which will
;; put that code in the heap
(define (quote-code asm)
  (define bytes (assemble asm))
  (define words (split-into-chunks 8 bytes))
  (define movs
    (apply append
           (map (lambda (bytes)
                  (seq
                   (Mov scratch-reg-1 (bytes-to-word bytes))
                   (Mov (Offset heap-reg 0) scratch-reg-1)
                   (Add heap-reg 8)))
                words)))
  (seq movs ;; append code which appends null terminating zero at end
       (Mov scratch-reg-1 0)
       (Mov (Offset heap-reg 0) scratch-reg-1)
       (Add heap-reg 8)))

;; List int -> int
(define (bytes-to-word bytes)
  (define (impl backwards)
    (match backwards
      ['() 0]
      [(cons byte rest)
       (+ byte (* 256 (impl rest)))]))
  (impl (reverse bytes)))

;; modified from stackoverflow
;; pads end with zeros
(define (split-into-chunks n xs)
  (if (= (length xs) 0)
      '()
      (if (< (length xs) n)
        (list (append xs (make-list (- n (length xs)) 0)))
        (let ((first-chunk (take xs n))
              (rest (drop xs n)))
          (cons first-chunk (split-into-chunks n rest))))))











