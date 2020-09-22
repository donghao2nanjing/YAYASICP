#lang sicp 

;;; This is a special case of Netwon method.

(define (sqrt_iter guess last_guess x)
    (if (good_enough? guess last_guess) guess 
        (sqrt_iter (improve guess x) guess x)
    ))

(define (improve guess x) 
    (average guess (/ x guess)))

(define (average x y) 
    (/ (+ x y) 2) )

(define (good_enough? guess last_guess) 
    (< (abs (- guess last_guess) ) 0.0000001) )


(sqrt_iter 5.0 10 100000000.0)
(sqrt_iter 8.0 10 0.01)
;;; Yes, it's better!