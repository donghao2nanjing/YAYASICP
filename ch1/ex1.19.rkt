#lang sicp 

(define (even? n) (= (remainder n 2) 0))

(define (fib n)
    (fib_iter 1 0 0 1 n)
)

;;; You need some calculation in this exercise :).
(define (fib_iter a b p q count)
    (cond ((= count 0 ) b)
        ((even? count) (
            fib_iter a b 
            (+ (* p p) (* q q)) ; compute p'
            (+ (* q q) (* 2 p q)) ; compute q'
            (/ count 2)
        ))
        (else (
            fib_iter 
                (+ (* b q) (* a q) (* a p))
                (+ (* b p) (* a q))
                p 
                q
                (- count 1)
        ))
    )
)

(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)