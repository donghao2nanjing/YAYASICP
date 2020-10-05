#lang sicp 

(define (element-of-set? x set)
    (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))
    )
)

(define (adjoin-set x set)
    (cons x set)
)

(define (rm-set x set)
    (cond 
        ((null? set) nil)
        ((equal? x (car set)) (cdr set))
        (else (cons (car set) (rm-set x (cdr set))))
    )
)

(define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '()) 
        ((element-of-set? (car set1) set2) (cons (car set1) (intersection-set (cdr set1) (rm-set (car set1) set2))))
        (else (intersection-set (cdr set1) set2))
    )
)

(define (union-set set1 set2)
    (append set1 set2)
)

(define s1 '(1 2 3 4))
(define s2 '(3 4 5 6))

(union-set s1 s2)
(intersection-set s1 s2)

(define s3 '(1 2 3 4 3 4))
(define s4 '(3 4 3 5 6))

(union-set s3 s4)
(intersection-set s3 s4)
