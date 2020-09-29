#lang sicp 

(define (reverse l)
    (define (helper l result)
        (if (null? l)
            result
            (helper (cdr l) (cons (car l) result))
        )
    )
    (helper l nil)
)

(reverse (list 1 4 9 16 25))
(reverse (list (list 1 2) (list 3 4)))

;;; Use pair? to determine type of variable.
(define (deep-reverse l)
    (define (helper l result)
        (cond ((null? l) result)
            ((pair? l) (helper (cdr l) (cons (deep-reverse (car l)) result)) )
            (else l)
        )
    )
    (helper l nil)
)


(deep-reverse (list 1 4 9 16 25))
(deep-reverse (list (list 1 2) (list 3 4)))