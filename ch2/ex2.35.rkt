#lang sicp 

(define (accumulate op init seq)
    (if (null? seq)
        init
        (op (car seq) (accumulate op init (cdr seq)))
    )
)

(define (count-leaves t)
    (accumulate + 0 (map (lambda (x) (cond ((null? x) 0) 
                                            ((pair? x) (count-leaves x))
                                            (else 1))) t))
)

(count-leaves (list 1 2 3 4))
(count-leaves (list 1 (list 2 3 4)))
(count-leaves (cons (list 1 2) (list 3 4)))
(count-leaves (cons (list 1 2) (list 3 4 5)))
