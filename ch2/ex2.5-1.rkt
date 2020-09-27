#lang sicp 

(define (pow b n)
    (define (iter n result)
        (if (= n 0) result
        (iter (- n 1) (* b result)))
    )
    (iter n 1)
)

;; Test pow: 
(pow 2 3)

(define (num-cons a b)
    (cons a b)
)

(define (num-car n)
    (pow 2 (car n))
)

(define (num-cdr n)
    (pow 3 (cdr n))
)

(define (num-value n)
    (* 
        (num-car n)
        (num-cdr n)
    )
)

(define (num-product n1 n2)
    (num-cons
        (+ (car n1) (car n2)) 
        (+ (cdr n1) (cdr n2)) 
    )
)

(num-value (num-cons 1 1))
(num-value (num-product (num-cons 1 1) (num-cons 2 2)))
