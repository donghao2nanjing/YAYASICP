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

(define (old-mul-interval x y) 
    (let (
            (p1 (* (lower-bound x) (lower-bound y)))
            (p2 (* (lower-bound x) (upper-bound y)))
            (p3 (* (upper-bound x) (lower-bound y)))
            (p4 (* (upper-bound x) (upper-bound y)))
        )
        (make-interval 
            (min p1 p2 p3 p4)
            (max p1 p2 p3 p4)
        )
    )
)

(define (positive? x) 
    (> (lower-bound x) 0)
)

(define (negative? x) 
    (< (upper-bound x) 0)
)

(define (spanzero? x)
    (and (not (positive? x)) (not (negative? x) ))
)


(define (mul-interval x y)
    (cond 
        ( (and (positive? x) (positive? y)) 
            (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))
        )
        ( (and (negative? x) (negative? y))
            (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))
        )
        ( (and (spanzero? x) (spanzero? y))
            (old-mul-interval x y)
        )
        ( (and (positive? x) (negative? y))
            (make-interval (* (lower-bound y) (upper-bound x)) (* (upper-bound y) (lower-bound x)))
        )
        ( (and (negative? x) (positive? y))
            (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y)))
        )
        ( (and (negative? x) (spanzero? y))
            (make-interval (* (lower-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y)))
        )
        ( (and (negative? y) (spanzero? x))
            (make-interval (* (lower-bound y) (upper-bound x)) (* (lower-bound y) (lower-bound x)))
        )
        ( (and (positive? x) (spanzero? y))
            (make-interval (* (upper-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))
        )
        ( (and (positive? y) (spanzero? x))
            (make-interval (* (upper-bound y) (lower-bound x)) (* (upper-bound y) (upper-bound x)))
        )
    )
)

(define (div-interval x y) 
    (if (spanzero? y)
        (error "interval y spans 0: " y)
    )
    (mul-interval x
        (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y)))
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
    (make-interval (- x_up y_low) (- x_low y_up))
    )
)

(sub-interval
    (make-interval 3 5)
    (make-interval 1 3)
)

(define (width-interval interval)
    (- (upper-bound interval) (lower-bound interval))
)

(width-interval 
    (add-interval 
        (make-interval 1 5)
        (make-interval 2 5)
    )
)

(mul-interval (make-interval 1 10) (make-interval 1 10) )
(mul-interval (make-interval 1 10) (make-interval -1 1) )
(mul-interval (make-interval 1 10) (make-interval -1 -10) )
(mul-interval (make-interval -1 1) (make-interval 1 10) )
(mul-interval (make-interval -1 1) (make-interval -1 1) )
(mul-interval (make-interval -1 1) (make-interval -1 -10) )
(mul-interval (make-interval -1 -10) (make-interval 1 10) )
(mul-interval (make-interval -1 -10) (make-interval -1 1) )
(mul-interval (make-interval -1 -10) (make-interval -1 -10) )