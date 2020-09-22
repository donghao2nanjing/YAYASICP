#lang sicp 

;;; This is linear iteration.
(define (+ a b) 
    (if (= a 0) b (+ (dec a) (inc b)))
)

(+ 4 5)