#lang sicp

(define (square x) (* x x))
(define (sum_square a b) (+ (square a) (square b)))

(define (sum_square_larger_two_from_three a b c)
    ( cond ((and (> a b) (> c b)) (sum_square a c))
        ((and (> a c) (> b c)) (sum_square a b))
        ((and (> b a) (> c a)) (sum_square b c))
    ))

(sum_square_larger_two_from_three 1 2 3)
(sum_square_larger_two_from_three 3 2 1)
(sum_square_larger_two_from_three 2 1 3)