#lang sicp 

(define (double n) (* n 2))
(define (halve n) (/ n 2))
(define (even? n) (= (remainder n 2) 0))

;;; recursion implementation
(define (fast_mul b n) 
    (cond 
        ((= n 1) b)
        ((even? n) (fast_mul (double b) (halve n)) )
        (else (+ b (fast_mul b (- n 1))))
    )
)

(fast_mul 3 5)
(fast_mul 6 6)