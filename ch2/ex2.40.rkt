#lang sicp

(define (enumerate-interval start end)
    (if (> start end) nil
        (cons start (enumerate-interval (+ start 1) end))
    )
)

(enumerate-interval 1 5)


(define (accumulate op init seq)
    (if (null? seq)
        init
        (op (car seq) (accumulate op init (cdr seq)))
    )
)

(define (unique-pair n)
    (accumulate append nil 
        (map 
            (lambda (i) (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n)
        )
    )
)


(define (smallest_divisor n) (find_divisor n 2))
(define (divides? d n) (= (remainder n d) 0))
(define (square x) (* x x))

(define (find_divisor n d)
    (cond ((> (square d) n) n)
        ((divides? d n) d) 
        (else (find_divisor n (+ d 1)))
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

(define (prime? n) (= n (smallest_divisor n)))

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair)))
)

(define (make-pair-sum pair) 
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair)))
)

(define (prime-sum-pairs n)
    (map make-pair-sum (filter prime-sum? (unique-pair n)))
)

(unique-pair 3)
(unique-pair 5)

(filter prime-sum? (unique-pair 5))
(prime-sum-pairs 5)
