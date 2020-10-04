#lang sicp 

(define (equal? a b)
    (cond 
        ((and (pair? a) (pair? b)) 
            (and (eq? (car a) (car b)) (equal? (cdr a) (cdr b)))
        )
        ((and (not (pair? a)) (not (pair? b)))
            (eq? a b)
        )
        (else #f)
    )
)

(equal? 
    (list 1 2 3 4 5)
    (list 1 2 3 4 5)
)

(equal? 
    (list 1 2 3 4 5)
    (list 1 2 5 4 5)
)
