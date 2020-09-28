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

(define (sub-interval x y)
    (let (
        (x_low (lower-bound x))
        (y_low (lower-bound y))
        (x_up (upper-bound x))
        (y_up (upper-bound y))
    )
    (make-interval (- x_low y_up) (- x_up y_low) )
    )
)

(sub-interval
    (make-interval 3 5)
    (make-interval 1 3)
)

