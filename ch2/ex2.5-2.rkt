#lang sicp 

(define (cons x y)
    (* (expt 2 x) (expt 3 y))
)

(define (car n)
    (define (helper n)
        (if (= 0 (remainder n 3)) 
            (helper (/ n 3))
            n
        )
    )
    (helper n)
)

(define (cdr n)
    (define (helper n)
        (if (= 0 (remainder n 2)) 
            (helper (/ n 2))
            n
        )
    )
    (helper n)
)

(car (cons 1 2))
(cdr (cons 1 2))