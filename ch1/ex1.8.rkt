#lang sicp

;;; cube root

(define (cuberoot guess x)
    (if (good_engough? guess x) guess
        (cuberoot (improve guess x) x) ))

(define (improve guess x)
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (good_engough? croot y) 
    (< (abs (- (* croot croot croot) y)) 0.001))

(cuberoot 1.0 27.0)