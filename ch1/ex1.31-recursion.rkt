#lang sicp 

(define (factorial term a next b)
    (if (> a b) 
        1.0
        (* (term a) (factorial term (next a) next b))    
    )
)

(define (next_n n) (+ n 2))
(define (test n) (/ (* (+ n 1) (- n 1)) (* n n)))

; pi/4 approximates 0.78539815
(factorial test 3 next_n 100) 
(factorial test 3 next_n 1000) 
(factorial test 3 next_n 10000) 
; Results:
; 0.7893349223043911
; 0.785790960604801
; 0.7854374342873239
