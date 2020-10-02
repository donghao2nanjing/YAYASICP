#lang sicp 

(define (make-vect x y)
    (list x y)
)

(define (xcor-vect v)
    (car v)
)

(define (ycor-vect v)
    (cadr v)
)

(define (add-vect v1 v2)
    (make-vect
        (+ (xcor-vect v1) (xcor-vect v2))
        (+ (ycor-vect v1) (ycor-vect v2))
    )
)

(define (sub-vect v1 v2)
    (make-vect
        (- (xcor-vect v1) (xcor-vect v2))
        (- (ycor-vect v1) (ycor-vect v2))
    )
)

(define (scale-vect v s)
    (make-vect
        (* s (xcor-vect v))
        (* s (ycor-vect v))
    )
)

(define (make-segment v1 v2)
    (list v1 v2)
)

(define (start-segment s)
    (car s)
)

(define (end-segment s)
    (cadr s)
)
