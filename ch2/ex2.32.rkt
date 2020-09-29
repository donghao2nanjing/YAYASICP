#lang sicp 

(define (subsets s)
    (if (null? s)
        (list nil) 
        (let ((rest (subsets (cdr s))))
            (append rest (map 
                (lambda (x) (cons (car s) x))
            rest))
        )
    )
)

(subsets (list 1 2 3))

; Why is works? 
; A subset of a set s is the (car s) in combination with rest + rest.
