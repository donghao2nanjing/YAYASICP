#lang sicp 

(define (accumulate op init seq)
    (if (null? seq)
        init
        (op (car seq) (accumulate op init (cdr seq)))
    )
)

(define (horner-eval x coeff-seq)
    (accumulate (lambda (this-coeff higher-terms) 
        (+ this-coeff (* x higher-terms))
        ) 
        0 
        coeff-seq
    )
)

(horner-eval 2 (list 1 3 0 5 0 1))
(horner-eval 1 (list 1 3 0 5 0 1))
