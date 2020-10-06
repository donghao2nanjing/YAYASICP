#lang sicp 

(define (element-of-set? x set)
    (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false) 
        (else (element-of-set? x (cdr set)))
    )
)

(define (intersection-set set1 set2)
    (if (or (null? set1) (null? set2)) '()
        (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2) (cons x1 (intersection-set (cdr set1) (cdr set2))))
                ((< x1 x2) (intersection-set (cdr set1) set2))
                ((> x1 x2) (intersection-set set1 (cdr set2)))
            )
        )
    )
)

(define (adjoin x set)
    (cond 
        ((null? set) (cons x set))
        ((< x (car set)) (cons x set))
        ((> x (car set)) (cons (car set) (adjoin x (cdr set))))
    )
)

(define (union-set set1 set2)
    (cond 
        ((null? set1) set2)
        ((null? set2) set1)
        (else
            (let 
                (
                    (x1 (car set1))
                    (x2 (car set2))
                )
                (cond 
                    ((> x1 x2) (cons x2 (union-set set1 (cdr set2))))
                    ((< x1 x2) (cons x1 (union-set set2 (cdr set1))))
                    (else (cons x1 (union-set (cdr set2) (cdr set1))))
                )
            )
        )
    )
)


(define s1 '(1 2 3 4))
(define s2 '(3 4 5 6))

(intersection-set s1 s2)
(union-set s1 s2)