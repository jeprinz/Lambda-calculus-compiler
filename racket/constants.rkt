#lang racket

(provide (all-defined-out))

(define heap-reg 'rbx)
(define scope-reg 'rax)
(define scratch-reg-1 'rcx)
(define scratch-reg-2 'rdx)
(define scratch-reg-3 'rdi)
(define return-reg 'r8)
;; (struct Variable () #:transparent)

(define int-mask 1)
(define ptr-mask 0)
(define mask 1)