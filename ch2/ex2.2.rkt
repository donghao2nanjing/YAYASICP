#lang sicp 

(define (print-point p) 
    (newline) 
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")")
)

(define (make-point x y)
    (cons x y)
)

(define (x-point p)
    (car p)
)

(define (y-point p)
    (cdr p)
)

(define (make-segment start end)
    (cons start end)
)

(define (start-segment seg)
    (car seg)
)

(define (end-segment seg)
    (cdr seg)
)

(define (midpoint-seg seg)
    (make-point 
        (/ (+ (x-point (start-segment seg)) (x-point (end-segment seg))) 2)
        (/ (+ (y-point (start-segment seg)) (y-point (end-segment seg))) 2)
    )
)

(print-point (midpoint-seg (make-segment 
    (make-point 2 4)
    (make-point 8  -4)
    ))
)
