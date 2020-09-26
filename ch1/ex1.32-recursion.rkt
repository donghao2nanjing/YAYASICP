#lang sicp 

(define (identity n) n)

(define (accumulate combiner null-value term a next b) 
    (if (> a b) 
        null-value
        (combiner (term a) (accumulate combiner null-value term (next a) next b))    
    )
)

(accumulate + 0 identity 1 inc 5)
(accumulate * 1 identity 1 inc 5)