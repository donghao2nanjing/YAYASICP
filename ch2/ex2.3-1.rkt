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

;;; rectangle, represented by points

(define (rectangle-perimeter rec)
    (* 2 (+ (x-length rec) (y-length rec)))
)

(define (rectangle-area rec)
    (* (x-length rec) (y-length rec))
)

(define (make-rectangle p-left-bottom p-right-top)
    (cons 
        p-left-bottom
        p-right-top
    )
)

(define (x-length rec)
    (abs (- 
        (x-point (car rec))
        (x-point (cdr rec))
    ))
)

(define (y-length rec)
    (abs (- 
        (y-point (car rec))
        (y-point (cdr rec))
    ))
)

(rectangle-perimeter (make-rectangle (make-point 1 3) (make-point 2 4)))
(rectangle-area (make-rectangle (make-point 1 3) (make-point 2 4)))
