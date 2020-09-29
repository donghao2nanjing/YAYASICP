#lang sicp 

(define (make-mobile left right) (list left right))

(define (make-branch length structure) (list length structure))

(define (left-branch m) (car m) )
(define (right-branch m) (car (cdr m)))
(define (branch-length b) (car b))
(define (branch-structure b) (car (cdr b)))

(define m1 (make-mobile
    (make-branch 1 2)
    (make-branch 1 2)
))

(define (b-weight b) 
    (cond
        ((number? (branch-structure b)) (branch-structure b))
        (else (total-weight (branch-structure b)))
))

(define (total-weight m)
    (+ (b-weight (left-branch m)) (b-weight (right-branch m)))
)

(define m2 (make-mobile
    (make-branch 1 3)
    (make-branch 1 m1)
))

(total-weight m1)
(total-weight m2)

(define (balanced-mobile m) 
    (= (b-weight (left-branch m)) (b-weight (right-branch m)))
)

(balanced-mobile m1)
(balanced-mobile m2)