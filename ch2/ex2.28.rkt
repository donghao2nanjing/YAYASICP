#lang sicp 

(define x (list (list 1 2) (list 3 4)))

(define (fringe x)
    (define (helper x result)
        (cond ( (not (pair? x)) (cons x result)) 
            ((null? (cdr x) ) (helper (car x) result))
            (else
                (append (helper (car x) nil) (helper (cdr x) result))
            )
        )
    )
    (helper x nil)
)

(fringe x)
(fringe (list x x))