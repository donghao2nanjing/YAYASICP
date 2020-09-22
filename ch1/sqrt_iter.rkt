#lang sicp 

;;; This is a special case of Netwon method.

(define (sqrt_iter guess x)
    (if (good_enough? guess x) guess 
        (sqrt_iter (improve guess x) x)))

(define (improve guess x) 
    (average guess (/ x guess)))

(define (average x y) 
    (/ (+ x y) 2) )

(define (good_enough? guess x) 
    (< (abs (- (square guess) x)) 0.001) )

(define (square x) (* x x))

(sqrt_iter 5.0 100000000.0)
(sqrt_iter 8.0 0.01)