#lang sicp 

(define (identity n) n)

(define (accumulate combiner null-value term a next b) 
    (define (iter a result)
        (if (> a b) 
            result
            (iter (next a) (combiner (term a) result))
        )
    )
    (iter a null-value)
)

(accumulate + 0 identity 1 inc 5)
(accumulate * 1 identity 1 inc 5)