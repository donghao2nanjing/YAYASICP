#lang sicp 

(define (cons x y)
    (lambda (m) (m x y))
)

(define (car z)
    (z (lambda (p q) p))
)

(define (cdr z)
    (z (lambda (p q) q))
)

(car (cons 1 2))
(cdr (cons 1 2))
; It's subtle here. Using substitution mode really helps you to understand.
