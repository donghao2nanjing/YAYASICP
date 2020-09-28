#lang sicp 

(define (for-each f l)
    (define (f-for-each f l)
        (f (car l))
        (for-each f (cdr l))
    )
    (if (null? l) #t
        (f-for-each f l)
    )
)

(for-each (lambda (x) (newline) (display x)) (list 57 321 88))
