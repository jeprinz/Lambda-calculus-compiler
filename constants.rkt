#lang racket

(provide (all-defined-out))

(define heap-reg 'rbx)
(define arg-reg 'rax)
(define scratch-reg-1 'rcx)
(struct Variable () #:transparent)