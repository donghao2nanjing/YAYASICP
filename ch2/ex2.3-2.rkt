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

;;; rectangle

(define (rectangle-perimeter rec)
    (* 2 (+ (x-length rec) (y-length rec)))
)

(define (rectangle-area rec)
    (* (x-length rec) (y-length rec))
)

; another representation
(define (make-rectangle x1 x2 y1 y2)
    (cons 
        (cons x1 y1)
        (cons x2 y2)
    )
)

(define (x-length rec)
    (abs (- 
        (car (car rec))
        (car (cdr rec))
    ))
)

(define (y-length rec)
    (abs (- 
        (cdr (car rec))
        (cdr (cdr rec))
    ))
)

(rectangle-perimeter (make-rectangle 1 2 3 4))
(rectangle-area (make-rectangle 1 2 3 4))
