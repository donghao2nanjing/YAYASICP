#lang sicp

;;; This procedure is used to demonstrate linear recursion.
(define (factorial n)
    (if (= n 1) 
        1
        (* n (factorial (- n 1)))
    ))

(factorial 6)