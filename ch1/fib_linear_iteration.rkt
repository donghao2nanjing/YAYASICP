#lang sicp

(define (fib n)
    (define (fib_iter a b count) 
        (if (= count 0) b
            (fib_iter (+ a b) a (- count 1))))
    (fib_iter 1 0 n))

(fib 0)
(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)