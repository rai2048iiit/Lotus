

#lang eopl


;; num : number? -> ast?
;(define num
;  (lambda (n)
;    (list 'number n))) 
;(num 4)
;; bool : boolean? -> ast?
;(define bool
;  (lambda (n)
;    (list 'boolean n)))
;
;; prim-app : prim-op? ast? ast? -> ast?
;(define prim-app
;  (lambda (prim-op ast1 ast2)
;    (list 'prim-app prim-op ast1 ast2)))
;
(define prim-ops (list + - * /  = < > ))
;
;; prim-op? : any? -> boolean?
(define prim-op?
  (lambda (op)
    (memq op prim-ops)))

;(define conditionals (list if))
; conditional? : any? -> boolean?
;(define conditional?
;  (lambda (thing)
;    (member thing conditionals)))


;(define op-symbol?
;  (lambda(x)
;    (member x prim-ops)))

(define-datatype ast ast?
  [num (datum number?)]
  [bool (datum boolean?)]
  [mylist (restnums (list-of ast?))]
  [prim-app (op prim-op?)
            (rands (list-of ast?))]
  [id-ref (sym symbol?)]
  [if-exp (test-exp ast?) (true-exp ast?) (false-exp ast?)]
  [! (unary-exp ast?)]  
  [add3 (num1 ast?) (num2 number?) (num3 number?)]
  [AND (exps (list-of ast?))]
  )

;(define apply-prim-op
;  (lambda (op 

; true-value? : number? -> boolean?
; returns true if the number is other than zero
(define true-value?
  (lambda (x)
    (not(zero? x))))
 

;eval-ast : ast? -> value?
(define eval-ast
  (lambda (a)
    (cases ast a
      [num (datum) datum]
      [bool (datum) datum]
      [mylist ( restnums) 
              (list (eval-ast (car restnums)) (eval-ast (car (cdr restnums))))]
      [prim-app (op rands) 
                (op (eval-ast (car rands)) (eval-ast (car (cdr rands))))]      
      [id-ref (sym) sym]
      [if-exp (test-exp true-exp false-exp)
          (cond
            [(true-value? (eval-ast test-exp)) (eval-ast true-exp)] 
            [else (eval-ast false-exp)])]
      [! (unary-exp) (not (eval-ast unary-exp))]
      
      [add3 (num1 num2 num3) (+ (eval-ast num1) (+ num2 num3))]
      [AND (exps)
           (cond 
             [(car exps) (eval-ast (AND (cdr exps)))]
             [else #f]
             )]
         
      )))
  



; parse : list? -> ast?
(define parse
  (lambda (exp)
    (cond
      [(number? exp) (num exp)]
      [(boolean? exp) (bool exp)] 
      [(and (list? exp)
            (= (length exp) 3)
            (prim-op? (car exp)))
            
            (let ([ast1 (parse (car (cdr exp)))]
                  [ast2 (parse (car (cdr (cdr exp))))])
              (prim-app (car exp) (list ast1 ast2)))]           
      [else eopl:error "Sorry Cannot Parse!"]
      )))


