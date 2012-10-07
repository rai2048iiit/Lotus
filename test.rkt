#lang racket
(define e1 (extended-env (list 'x) (list 5) (empty-env)))
(eval-ast (prim-app + (list (num 2) (id-ref e1 'x))) (empty-env))


(define e3  (extended-env '(x y z) '(1 2 3) (empty-env)))
(define e2  (extended-env '(w x) '(5 6) e3))
(eval-ast (assume (list (make-bind 'x (num 8))
                          (make-bind 'y (num 7)))
                    (prim-app + (list (id-ref 'x) (id-ref 'y)))) e2)

(eval-ast (function (list 'x 'y) (num 1)) (empty-env))