#lang racket

(require racket/cmdline)
(require "compile.rkt")

;; https://docs.racket-lang.org/reference/Command-Line_Parsing.html
(define output-file (make-parameter "a.out"))

(define file-to-compile
  (command-line
   #:program "compiler"
   ;;#:once-each
   ;;[("-v" "--verbose") "Compile with verbose messages"
   ;;                    (verbose-mode #t)]
   #:args (filename) ; expect one command-line argument: <filename>
   ; return the argument as a filename to compile
   filename))


(main file-to-compile (output-file))
