#lang sicp 

(define right-split (split beside below))
(define up-split (split below beside))

(define (split f1 f2)
    (lambda (painter) 
        (f1 painter (f2 painter painter) )
    )
)
