#lang racket

(define e1 (extended-env (list 'x) (list 5) (empty-env)))

; primitive application
(eval-ast (prim-app + (list (num 2) (id-ref e1 'x))) (empty-env))


(define e3  (extended-env '(x y z) '(1 2 3) (empty-env)))
(define e2  (extended-env '(w x) '(5 6) e3))

; LET
(eval-ast (assume (list (make-bind 'x (num 8))
                          (make-bind 'y (num 7)))
                    (prim-app + (list (id-ref 'x) (id-ref 'y)))) e2)
; FUNCTION

; returns closure
(eval-ast (function (list 'x 'y) (num 1)) (empty-env))

; returns value in the environment in which it is called
(define e5 (extended-env (list 'x 'y) (list 5 2) (empty-env)))
(define fun (function '(x) (id-ref 'x)))
(eval-ast (app fun (list (num 3))) e5)

