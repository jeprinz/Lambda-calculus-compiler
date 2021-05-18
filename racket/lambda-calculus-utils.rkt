#lang racket
(require plot)

;; Definition of type Exp
(define (lam e) (list 'lam e))
(define (app e1 e2) (list 'app e1 e2))
(define (var x) (list 'var x))
(define (const n) (list 'const n))
(define (plus a b) (list 'plus a b))

;; Definition of Sugar:
;; application is just (a b c d)
;; lambdas are (lam (x y z) e)
;;    OR   lam x e
;; variables are just the name of the variable

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

;; NOTE: does NOT handle free variables in v properly
;; Exp -> int -> Exp -> Exp
(define (sub e x v) ;; subs at debruin index x.
  (match e
    [(list 'app e1 e2)
     (app (sub e1 x v) (sub e2 x v))]
    [(list 'lam e)
     (lam (sub e (+ 1 x) v))]
    [(list 'var i)
     (if (eq? x i)
         v (var i))]
    [(list 'const n)
     (list 'const n)]
    [(list 'plus e1 e2)
     (plus (sub e1 x v) (sub e2 x v))]))

;; Exp -> Exp
(define (exp-eval e)
  (match e
    [(list 'app e1 e2)
     (let [(n1 (exp-eval e1))
           (n2 (exp-eval e2))]
       (match n1
         [(list 'lam n) ;; do beta-reduction
          (exp-eval
           (sub n 0 n2))]
         [other
          (app n1 n2)]))]
    [(list 'plus e1 e2)
     (let [(n1 (exp-eval e1))
           (n2 (exp-eval e2))]
       (match (list n1 n2)
         [(list (list 'const a) (list 'const b))
          (const (+ a b))]
         [other "plus incorrectly applied"]))]
    [(list 'const n)
     (const n)]
    [(list 'lam e)
     (lam e)]
    [(list 'var i)
     (raise "shouldn't get here unless free var")]))

;; int -> Sugar int
(define (church-numeral n)
  (define (apps n)
    (if (eq? n 0)
        'x
        `(f ,(apps (- n 1)))))
  `(lam (f x) ,(apps n)))

;; Exp -> Racket program
(define (to-racket e)
  (define (to-racket-impl e ctx)
    (match e
      [(list 'app e1 e2)
       `(,(to-racket-impl e1 ctx) ,(to-racket-impl e2 ctx))]
      [(list 'lam e)
       (let ((x (gensym)))
         `(lambda (,x) ,(to-racket-impl e (cons x ctx))))]
      [(list 'var i)
       (list-ref ctx i)]
      [(list 'const n)
       n]
      [(list 'plus e1 e2)
       `(+ ,(to-racket-impl e1 ctx) ,(to-racket-impl e2 ctx))]))
  (to-racket-impl e '()))

;; Exp -> Javascript program
(define (to-js e)
  (define (to-js-impl e ctx)
    (match e
      [(list 'app e1 e2)
       (string-append (to-js-impl e1 ctx)
                      "(" (to-js-impl e2 ctx) ")")]
      [(list 'lam e)
       (let ((x (symbol->string (gensym))))
         (string-append "((" x ") => ("
                        (to-js-impl e (cons x ctx))
                        "))"))]
      [(list 'var i)
       (list-ref ctx i)]
      [(list 'const n)
       (number->string n)]
      [(list 'plus e1 e2)
       (string-append "(" (to-js-impl e1 ctx)
                      "+" (to-js-impl e2 ctx) ")")]))
  (to-js-impl e '()))

;; Sugar (nat -> nat -> nat)
(define add
  `(lam (n m f x)
        (n f (m f x))))

(define mul
  `(lam (n m f x)
        (n (m f) x)))

(define pow
  `(lam (n m)
        (m (,mul n) ,(church-numeral 1))))

;; Sugar nat
(define (prog1 n)
  `(,add ,(church-numeral n) ,(church-numeral n) (lam x (+ x 1)) 0))

(define (prog2 n)
  `(,pow ,(church-numeral n)
         (,pow ,(church-numeral n)
               (,pow ,(church-numeral n)
                     ,(church-numeral n)))
    (lam x (+ x 1)) 0))

(define (prog3 n)
  `(,pow ,(church-numeral 3)
         (,pow ,(church-numeral 2)
               (,pow ,(church-numeral 2)
                     ,(church-numeral 2)))
    (lam x (+ x 1)) 0))

(define prog3to13
  `(,pow ,(church-numeral 3) ,(church-numeral 13)
         (lam x (+ x 1)) 0))

(define (write-to-file name data)
  (define out (open-output-file name))
  (display data out)
  (close-output-port out))

(define (time-rkt prog)
  (let ((code (eval `(lambda () ,(to-racket (desugar prog))))))
    (time (code))))

(define (time-sub prog n)
  (let ((code (desugar (prog n))))
    (time (exp-eval code))))

(define (rak-ack n m)
  (if (= n 0)
      (+ m 1)
      (if (= m 0)
          (rak-ack (- n 1) 1)
          (rak-ack (- m 1) (rak-ack m (- n 1))))))

(define pair
  `(lam (x y) (lam f (f x y))))

(define true
  `(lam (x y) x))

(define false
  `(lam (x y) y))

;(define fst
;  `(lam p (p true)))


(define pred
  `(lam n
        (n (lam p (p ,false
                     (,pair ,(church-numeral 0) ,false)
                     (,pair (,add (p ,true) ,(church-numeral 1)),false)))
                (,pair ,0 ,true)
                ,true)))

(define is-zero
  `(lam n
        (n (lam x ,false) ,true)))

;(define safe-pred
;  `(lam n
;        (,is-zero n ,(church-numeral 0) (,pred n))))

(define subtract
  `(lam (m n)
        (n ,pred m)))

(define ex1
  `(,pred ,(church-numeral 0) (lam x (+ x 1)) 0))

(define progsub
  `(,subtract (,pow ,(church-numeral 4) ,(church-numeral 5))
              (,pow ,(church-numeral 3) ,(church-numeral 5))
              (lam x (+ x 1)) 0))




    