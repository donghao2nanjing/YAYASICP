#lang sicp 

(define (gcd a b)
    (if (= b 0)  a 
    (gcd b (remainder a b)))
)

(define numer car)
(define denom cdr)

(define (make-rat n d)
    (define (sign n d) 
        (if (< (* n d) 0) -1 1 )
    )
    (let (
            (n_abs (abs n))
            (d_abs (abs d))
        )
        (let 
            (
                (g (gcd n_abs d_abs))
                (s (sign n d))
            )
            (cons (/ n_abs g) (* s (/ d_abs g)))
        )
    )
)

(define (print-rat x) 
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x))
)

(print-rat (make-rat -2 -8))
(print-rat (make-rat -2 8))
(print-rat (make-rat 2 -8))
(print-rat (make-rat 2 8))