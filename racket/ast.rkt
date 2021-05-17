#lang racket

(provide desugar delambda Elam Eapp Evar Econst Eplus
         IntExp Ivar Iconst Icall Iclosure Iplus)

;; Definition of type Exp
(struct Elam (e) #:transparent)
(struct Eapp (e1 e2) #:transparent)
(struct Evar (x) #:transparent) ;; debruin index
(struct Econst (n) #:transparent)
(struct Eplus (a b) #:transparent)

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
             (Elam (lam-stuff (cdr names) e (cons (car names) ctx)))))
       (if (list? name)
           (lam-stuff name e ctx)
           (Elam (desugarImpl e (cons name ctx))))]
      [x
       #:when (symbol? x)
       (Evar (index-of ctx x))]
      [i
       #:when (number? i)
       (Econst i)]
      [l
       #:when (eq? (car l) '+)
       (Eplus (desugarImpl (list-ref l 1) ctx) (desugarImpl (list-ref l 2) ctx))]
      [l ;; app case, hopefully...
       #:when (list? l)
       (define (app-stuff l)
           (if (eq? (length l) 1)
               (car l)
               (Eapp (app-stuff (cdr l)) (car l))))
         (app-stuff (map (lambda (e) (desugarImpl e ctx)) (reverse l)))]
      [other
       (println "error in desugar:")
       (raise e)]))
  (desugarImpl e null))

;; An intermediate language, IntExp, which is a List (Label, Exp')
;; Exp'

;; Exp'
(struct Icall (c1 c2) #:transparent) ;; represents calling a closure with an argument
(struct Iclosure (label) #:transparent) ;; represents creation of a function with access to local scope
(struct Iplus (e1 e2) #:transparent) ;; addition
(struct Iconst (n) #:transparent)
(struct Ivar (x) #:transparent)

(struct IntExp (main functions) #:transparent)

;; Exp -> intExp (Exp' , List (Label, Exp'))
(define (delambda e)
  (match e
    [(Elam e)
     (let* [(l (gensym))
            (res (delambda e))]
       (IntExp (Iclosure l)
             (append (list (cons l (IntExp-main res))) (IntExp-functions res))))]
    [(Eapp e1 e2)
     (let* [(res1 (delambda e1))
            (res2 (delambda e2))]
       (IntExp (Icall (IntExp-main res1) (IntExp-main res2))
             (append (IntExp-functions res1) (IntExp-functions res2))))]
    [(Evar x) (IntExp (Ivar x) '())]
    [(Econst n) (IntExp (Iconst n) '())]
    [(Eplus e1 e2)
     (let* [(res1 (delambda e1))
            (res2 (delambda e2))]
       (IntExp (Iplus (IntExp-main res1) (IntExp-main res2))
             (append (IntExp-functions res1) (IntExp-functions res2))))]))


       
     












