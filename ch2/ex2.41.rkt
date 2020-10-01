#lang sicp

(define (enumerate-interval start end)
    (if (> start end) nil
        (cons start (enumerate-interval (+ start 1) end))
    )
)

(define (accumulate op init seq)
    (if (null? seq)
        init
        (op (car seq) (accumulate op init (cdr seq)))
    )
)

(define (unique-triples n)
    (accumulate append nil
        (accumulate append nil 
            (map 
                (lambda (i) 
                    (map 
                        (lambda (j) 
                            (map
                                (lambda (k) (list i j k))
                                (enumerate-interval 1 (- j 1))
                            )
                        )
                        (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)
            )
        )
    )
)

(define (filter proc l)
    (if (null? l) nil
        (if (proc (car l)) 
            (cons (car l) (filter proc (cdr l)))
            (filter proc (cdr l))
        )
    )
)

(define (sum-eq? triple n)
    (=
        (+ (car triple) (cadr triple) (cadr (cdr triple) ))
        n
    )
)

(define (find-triple n s) 
    (filter (lambda (triple) (sum-eq? triple s)) (unique-triples n))
)

(unique-triples 5)

(car (unique-triples 5))
(sum-eq? (car (unique-triples 5) ) 6)

(find-triple 6 15)
