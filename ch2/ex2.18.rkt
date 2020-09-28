#lang sicp 

(define (reverse l)
    (define (helper l result)
        (if (null? (cdr l))
            (cons (car l) result)
            (helper (cdr l) (cons (car l) result))
        )
    )
    (helper l nil)
)

(reverse (list 1 4 9 16 25))
