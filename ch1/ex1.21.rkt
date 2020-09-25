#lang sicp 

(define (smallest_divisor n) (find_divisor n 2))
(define (divides? d n) (= (remainder n d) 0))
(define (square x) (* x x))

(define (find_divisor n d)
    (cond ((> (square d) n) n)
        ((divides? d n) d) 
        (else (find_divisor n (+ d 1)))
    )
)

(smallest_divisor 199)
(smallest_divisor 1999)
(smallest_divisor 19999)