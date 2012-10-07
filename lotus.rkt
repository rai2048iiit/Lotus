
#lang scheme
(require eopl/eopl)

(define prim-ops (list + - * /  = < > ))

;
;; prim-op? : any? -> boolean?
(define prim-op?
  (lambda (op)
    (memq op prim-ops)))


(define-datatype env env?
  [empty-env]
  [extended-env (syms (list-of symbol?))
                (vals (list-of number?))
                (outer-env env?)])

(define list-index
  (lambda (l x ind)
    (cond
      [(null? l) (let ([i -1]) i)]
      [(equal? x (car l)) ind]
      [else (list-index (cdr l) x (+ 1 ind))])))

(define lookup-env
  (lambda (e x)
    (cases env e
      [empty-env () (eopl:error "Sorry not found!")]
      [extended-env (syms vals outer-env)
                    (let ([i (list-index syms x 0)])
                      (cond
                        [(= i -1) (lookup-env outer-env x)]
                        [else (list-ref vals i)]))])))


(define-datatype bind bind?
  [make-bind (b-id symbol?) (b-ast ast?)])

(define-datatype proc proc?
  [prim-proc (prim-op procedure?)
             (sig (list-of procedure?))]
  [closure (formals (list-of symbol?))
           (body ast?)
           (env env?)])

(define-datatype ast ast?
  [num (datum  number?)]
  [bool (datum boolean?)]
  [mylist (restnums (list-of ast?))]
  [prim-app (op prim-op?)
            (rands (list-of ast?))]
  [id-ref (sym symbol?)]
  [if-exp (test-exp ast?) (true-exp ast?) (false-exp ast?)]
  [! (unary-exp ast?)]  
  [add3 (num1 ast?) (num2 number?) (num3 number?)]
  [AND (exps (list-of ast?))]  
  [assume (binds (list-of bind?)) (body ast?)]
  [function (formals (list-of symbol?))
            (body ast?)]
  [app (rator ast?) (rands (list-of ast?))]
  )



; true-value? : number? -> boolean?
; returns true if the number is other than zero
(define true-value?
  (lambda (x)
    (not(zero? x))))

;;; bind-id : bind? -> id?
(define bind-id
  (lambda (b)
    (cases bind b
      [make-bind (b-id b-ast) b-id])))

;;; bind-ast : bind? -> ast?
(define bind-ast
  (lambda (b)
    (cases bind b
      [make-bind (b-id b-ast) b-ast])))

;eval-ast : ast? -> value?
(define eval-ast
  (lambda (a env)
    (cases ast a
      [num (datum) datum]
      [bool (datum) datum]
      [mylist ( restnums) 
              (list (eval-ast (car restnums)) (eval-ast (car (cdr restnums))))]
      [prim-app (op rands) 
                (let ([args (map 
                             (lambda(rand) 
                               (eval-ast rand env)) 
                             rands)])
                  (apply op args))]
      [id-ref (sym) (lookup-env env sym)]
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
      [assume (binds body)
              (let* ([ids (map bind-id binds)]
                     [asts (map bind-ast binds)]
                     [vals (map (lambda (a)
                                  (eval-ast a env))
                                asts)]
                     [new-env
                      (extended-env ids vals env)])
                
                (eval-ast body new-env))]        
      [function (formals body)
                (closure formals body env)]
      [app (rator rands)
           (let ([p (eval-ast rator env)]  
                 [args (map 
                        (lambda (rand)
                          (eval-ast rand env))
                        rands)])
             (if (proc? p)
                 (apply-proc p args)
                 (eopl:error "Rator must be proc?")))]
      
      )))

(define apply-proc
  (lambda (p args)
    (cases proc p
      [prim-proc (prim sig)
                 (apply-prim-proc prim sig args)]
      [closure (formals body env)
               (apply-closure formals body env args)]
      )))

(define apply-prim-proc
  (lambda (prim sig args)
    (let ([return-type (first sig)]
          [arg-types (rest sig)])
      (cond [(and (= (length arg-types) (length args))
             (andmap (match-arg-type (arg-types args))))
             
             (apply prim args)]
            
            [else (error)]))))

(define match-arg-type
  (lambda (type arg)
    (type arg)))

(define apply-closure
  (lambda(formals body env args)
    (let ([new-env (extended-env formals args env)])
      (eval-ast body new-env))))


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


