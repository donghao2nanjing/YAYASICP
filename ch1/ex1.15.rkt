#lang sicp

(define (cube x) (* x x x))
(define (p x) 
    (- (* 3 x) (* 4 (cube x)))
)

(define (sine angle) 
    (if (not (> (abs angle) 0.1)) angle (p (sine (/ angle 3.0))))
)

;;; p applied for 5 times.
(sine 12.15)

;;; order of growth in space: O(log(n))
;;; order of growth in steps: O(log(n))