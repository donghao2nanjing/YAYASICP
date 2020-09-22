#lang sicp 

(define (newif predicate then_do else_do) 
    (cond (predicate then_do) 
        (else else_do)))

(newif (= 2 3) 0 5)
(newif (= 1 1) 0 5)

(define (sqrt_iter guess x)
    (newif (good_enough? guess x) guess 
        (sqrt_iter (improve guess x) x)))

(define (improve guess x) 
    (average guess (/ x guess)))

(define (average x y) 
    (/ (+ x y) 2) )

(define (good_enough? guess x) 
    (< (abs (- (square guess) x)) 0.001) )

(define (square x) (* x x))

(sqrt_iter 5.0 100.0)

;;; The interpreter never return due to applicative order.
;;; ATTENTION: the computer may stuck after a while due to continuous evaluating.