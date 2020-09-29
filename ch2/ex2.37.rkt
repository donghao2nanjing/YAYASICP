#lang sicp 

(define (accumulate op init seq)
    (if (null? seq)
        init
        (op (car seq) (accumulate op init (cdr seq)))
    )
)

(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        nil 
        (cons (accumulate op init (accumulate (lambda (x y) (cons (car x) y)) nil seqs))
            (accumulate-n op init (accumulate (lambda (x y) (cons (cdr x) y)) nil seqs)))
    )
)

(define (map2 op v w)
    (if (null? v) 
        nil
        (cons (map op (car v) (car w)) (map2 op (cdr v) (cdr w)))
    )
)

(define (dot-product v w)
    (accumulate + 0 (map * v w))
)

(define (matrix-*-vector m v)
    (map (lambda (row) (dot-product row v)) m)
)

(define (transpose mat)
    (accumulate-n (lambda (x y) (cons x y)) nil  mat)
)

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map  
            (lambda (row) (matrix-*-vector cols row))
        m)
    )
)

(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define v (list 1 2 3 4))

(display m)
(newline)
(display v)
(newline)
(display (map2 * m m))
(newline)
(dot-product v v)
(newline)
(matrix-*-vector m v)
(newline)
(transpose m)
(newline)
(matrix-*-matrix m (transpose m))

(define a (list (list 1 2) (list 3 4)))
(matrix-*-matrix a (transpose a))
