#lang sicp 

(define (make-interval a b)
    (if (< a b) 
        (cons a b)
        (cons b a)
    )
)

(define (lower-bound interval)
    (car interval)
)

(define (upper-bound interval)
    (cdr interval)
)

(define (add-interval x y)
    (make-interval 
        (+ (lower-bound x) (lower-bound y))
        (+ (upper-bound x) (upper-bound y))
    )
)

(add-interval 
    (make-interval 1 2)
    (make-interval 4 3)
)
