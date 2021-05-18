#lang racket

(provide main)
(require a86)
(require "ast.rkt")
(require "constants.rkt")

(define arg-reg 'r9)

(define (waste-time n) (make-list n (Mov 'rax 'rax)))

;; Checks if a function can be tail-call optimized, if so then does it
(define (compileExprTailCall e)
  (match e
    [(Icall c1 c2)
     (seq
      (compileExprGeneral c1) ;; Run c1, it's result is on stack
      (compileExprGeneral c2) ;; Run c2, it's result is on stack
      (Pop arg-reg) ;; argument to call
      (Pop scratch-reg-1) ;; scratch-reg-1 holds closure = ptr to 16 byte {function ptr, scope ptr}
      
      (check-closure scratch-reg-1) ;; check that it really is a function, and not an integer
      ;; Next, we build that Scope object for the function. It should be the scope object in
      ;; the closure pointed to by c1, along with our argument, c2
      (Mov scope-reg (Offset scratch-reg-1 8)) ;; get scope from closure
      (Mov scratch-reg-3 (Offset scratch-reg-1 0)) ;; get the function pointer to be called
      (Jmp scratch-reg-3) ;; call the function
      )]
    [else #f]))

;; breaks scratch-reg-3
(define (check-number reg)
  (seq
   (Mov scratch-reg-3 reg)
   (And scratch-reg-3 mask)
   (Cmp scratch-reg-3 int-mask)
   (Jne 'raiseError)))

;; breaks scratch-reg-3
(define (check-closure reg)
  (seq
   (Mov scratch-reg-3 reg)
   (And scratch-reg-3 mask)
   (Cmp scratch-reg-3 ptr-mask)
   (Jne 'raiseError)))

;; Compiles single function
;; Exp' -> a86
(define (compileExprGeneral e)
  (match e
    [(Iconst n) ;; push number to stack
     (seq
      (Push (+ (* n 2) int-mask)) ;; (* n 2) is the bitwise shift of n to the left, the +1 is to set the flag.
      )] ;; not correct, needs to shift add bit
    [(Iplus e1 e2) ;; pop top two from stack, add, push
     (seq ;; not correct, from old thing
      (compileExprGeneral e1)
      (compileExprGeneral e2)
      (Pop scratch-reg-1)
      (Pop scratch-reg-2)
      (check-number scratch-reg-1)
      (check-number scratch-reg-2)
      (Sar scratch-reg-1 1)
      (Sar scratch-reg-2 1)
      (Add scratch-reg-1 scratch-reg-2)
      (Sal scratch-reg-1 1)
      (Or scratch-reg-1 int-mask)
      (Push scratch-reg-1))]
    [(Iclosure label) ;; create closure (label, current data) in heap, push pointer to closure onto stack
     (seq
      (Lea scratch-reg-1 label)
      ;; Create scope including current argument
      (Mov (Offset heap-reg 0) scope-reg)
      (Mov (Offset heap-reg 8) arg-reg)
      ;; Create closure object
      (Mov (Offset heap-reg 16) scratch-reg-1)
      (Mov (Offset heap-reg 24) heap-reg)
      (Add heap-reg 16)
      (Push heap-reg)
      (Add heap-reg 16))]
    [(Ivar i) ;; travel down n pointers to nth element of data in arg-register. push to stack.
     (if (= i 0) ;; do we get the argument from scope or arg-reg
         (Push arg-reg)
         (seq
          (Mov scratch-reg-1 scope-reg)
          (make-list (- i 1) (Mov scratch-reg-1 (Offset scratch-reg-1 0)))
          (Mov scratch-reg-1 (Offset scratch-reg-1 8))
          (Push scratch-reg-1)))]
    [(Icall c1 c2) ;; Run c1, Run c2, results in stack, pop c2 to arg-register, call c1
     (seq
      (compileExprGeneral c1) ;; Run c1, it's result is on stack
      (compileExprGeneral c2) ;; Run c2, it's result is on stack
      (Mov scratch-reg-2 arg-reg)
      (Pop arg-reg) ;; argument to call
      (Pop scratch-reg-1) ;; scratch-reg-1 holds closure = ptr to 16 byte {function ptr, scope ptr}
      (Push scratch-reg-2) ;; Push own argument for afterwards
      (Push scope-reg) ;; Push own scope
      
      (check-closure scratch-reg-1) ;; check that it really is a function, and not an integer
      ;; Next, we build that Scope object for the function. It should be the scope object in
      ;; the closure pointed to by c1, along with our argument, c2
      (Mov scope-reg (Offset scratch-reg-1 8)) ;; get scope from closure
      (Mov scratch-reg-3 (Offset scratch-reg-1 0)) ;; get the function pointer to be called
      (Call scratch-reg-3) ;; call the function
      (Pop scope-reg) ;; get our scope back
      (Pop arg-reg) ;; get arg-reg back
      (Push return-reg) ;; put the result back on the stack
      )]))

(define (compileExpr e)
  (let [(tail-call-try (compileExprTailCall e))]
    (if tail-call-try
        tail-call-try
        (compileExprGeneral e))))

(define (compileImpl es)
  (match es
    [(IntExp body defs)
     (seq
      (compileExprGeneral body)
      (Jmp 'end)
      (apply append
             (map (lambda (def)
                    (match def
                      [(cons label body)
                       (seq (Label label) ;; start of function
                            (compileExpr body) ;; body
                            (Pop return-reg) ;; result is on stack
                            (Ret) ;; return
                            )])) defs)))]))

;; Exp -> a86
(define (compile es)
  (prog
   (Extern 'error)
   (Label 'entry)
   (Mov heap-reg 'rdi) ;; get heap ptr from main.c
   (compileImpl (delambda (desugar es)))
   (Label 'end)
   (Pop 'rax)
   (check-number 'rax)
   (Sar 'rax 1)
   (Ret)
   (Label 'raiseError)
   (Call 'error) ;; error from main.c
   ))


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