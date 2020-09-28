#lang sicp 

(define (reverse l)
    (define (helper l result)
        (if (null? (cdr l))
            (cons (car l) result)
            (helper (cdr l) (cons (car l) result))
        )
    )
    (helper l nil)
)

(define (same-parity a . l)
    (let ((r (remainder a 2)))
        (define (iter l result)
            (if (null? l) result
                (if (= r (remainder (car l) 2)) (iter (cdr l) (cons (car l) result ))
                    (iter (cdr l) result)
                )
            )
        )
        (cons a (reverse (iter l nil)))
    )
)

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
