#lang sicp 

(define (even? n) (= (remainder n 2) 0))

(even? 2)
(even? 23)

(define (exp_iter b n)
    (exp_iteration b 1 n)
)

(define (exp_iteration b a n) 
    (cond 
    ((= n 0) a)
    ((even? n) (exp_iteration (* b b) a (/ n 2) ))
    (else (exp_iteration b (* a b) (- n 1)))
    )
)

(exp_iter 2 2)
(exp_iter 3 3)
(exp_iter 4 4)