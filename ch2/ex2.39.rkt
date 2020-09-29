#lang sicp 

(define (accumulate op init seq)
    (if (null? seq)
        init
        (op (car seq) (accumulate op init (cdr seq)))
    )
)

(define (fold-right op initial seq) (accumulate op initial seq))

(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest) result
            (iter (op result (car rest)) (cdr rest))
        )
    )
    (iter initial sequence)
)

(define (reverse-r sequence) 
    (fold-right (lambda (x y) (append y (list x))) nil sequence)
)

(define (reverse-l sequence) 
    (fold-left (lambda (x y) (cons y x)) nil sequence)
)

(reverse-r (list 1 2 3 4))
(reverse-l (list 1 2 3 4))
