#lang racket

(provide desugar lam app var const plus)

;; Definition of type Exp
(define (lam e) (list 'lam e))
(define (app e1 e2) (list 'app e1 e2))
(define (var x) (list 'var x)) ;; debruin index
(define (const n) (list 'const n))
(define (plus a b) (list 'plus a b))

;; Definition of Sugar:
;; application is just (a b c d)
;; lambdas are (lam (x y z) e)
;;    OR   lam x e
;; variables are just the name of the variable
;;; (+ 1 3)

;; Sugar -> Exp
(define (desugar e)
  (define (desugarImpl e ctx)
    (match e
      [(list 'lam name e)
       (define (lam-stuff names e ctx)
         (if (empty? names)
             (desugarImpl e ctx)
             `(lam ,(lam-stuff (cdr names) e (cons (car names) ctx)))))
       (if (list? name)
           (lam-stuff name e ctx)
           (lam (desugarImpl e (cons name ctx))))]
      [x
       #:when (symbol? x)
       (var (index-of ctx x))]
      [i
       #:when (number? i)
       (const i)]
      [l
       #:when (eq? (car l) '+)
       (plus (desugarImpl (list-ref l 1) ctx) (desugarImpl (list-ref l 2) ctx))]
      [l ;; app case, hopefully...
       #:when (list? l)
       (define (app-stuff l)
           (if (eq? (length l) 1)
               (car l)
               (app (app-stuff (cdr l)) (car l))))
         (app-stuff (map (lambda (e) (desugarImpl e ctx)) (reverse l)))]
      [other
       (println "error in desugar:")
       (raise e)]))
  (desugarImpl e null))

;; A Third type, Code, which is a List (Label, Exp')
;; Exp'

(define (codeCall c1 c2) (list 'codeCall c1 c2)) ;; represents calling a closure with an argument
(define (codeClosure label) (list 'codeClosure label)) ;; represents creation of a function with access to local scope
(define (codePlus e1 e2) (list 'codePlus e1 e2)) ;; addition
(define (codeConst n) (list 'codeConst n))
(define (codeVar x) (list 'var x))

(struct Code (main functions) #:transparent)

;; Exp -> Code (Exp' , List (Label, Exp'))
(define (delambda e)
  (match e
    [(list 'lam e)
     (let* [(l (gensym))
            (res (delambda e))]
       (Code (codeClosure l)
             (append (cons l (Code-main res)) (Code-functions res))))]
    [(list 'app e1 e2)
     (let* [(res1 (delambda e1))
            (res2 (delambda e2))]
       (Code (codeCall (Code-main res1) (Code-main res2))
             (append (Code-functions res1) (Code-functions res2))))]
    [(list 'var x) (Code (codeVar x) '())]
    [(list 'const n) (Code (codeConst n) '())]
    [(list 'plus e1 e2)
     (let* [(res1 (delambda e1))
            (res2 (delambda e2))]
       (Code (codeCall (Code-main res1) (Code-main res2))
             (append (Code-functions res1) (Code-functions res2))))]))

       
     












