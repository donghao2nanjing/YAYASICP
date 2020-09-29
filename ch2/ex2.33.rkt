#lang sicp 

(define (accumulate op init seq)
    (if (null? seq)
        init
        (op (car seq) (accumulate op init (cdr seq)))
    )
)

(define (map p sequence)
    (accumulate (lambda (x y)  (cons (p x) y)) nil sequence)
)

(map (lambda (x) (+ x 1)) (list 1 2 3))

(define (append s1 s2)
    (accumulate cons s2 s1)
)

(append (list 1 2) (list 3 4))

(define (length seq)
    (accumulate (lambda (x y) (+ 1 y)) 0 seq)
)

(length (list 1 2 3 4))

;;; lambda表达式真的好啊
