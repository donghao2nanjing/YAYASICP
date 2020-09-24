#lang sicp 

(define (double n) (* n 2))
(define (halve n) (/ n 2))
(define (even? n) (= (remainder n 2) 0))

(define (fast_mul b n) 
    (mul_iter b n 0)
)

;;; iteration implementation
(define (mul_iter b n p) 
    (cond
        ((= n 0) p)
        ((even? n) (mul_iter (double b) (halve n) p))
        (else (mul_iter b (- n 1) (+ p b)))
    )
)

(fast_mul 3 5)
(fast_mul 6 6)