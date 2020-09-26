#lang sicp 

(define (sum term a next b)
    (if (> a b) 0
        (+ (term a) 
            (sum term (next a) next b))
    ) 
)

(define (integral f a b dx)
    (define (add_dx x) (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add_dx b) dx)
)

(define (inc n) (+ n 1))
(define (cube n) (* n n n))


(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

; Results: 
; 0.24998750000000042
; 0.249999875000001

(define (even? n) (= (remainder n 2) 0))

(define (integral_simpson f a b n) 
    (define (sum_simpson f a h n k)
        (cond ((= k 0)  (+ (f (+ a (* k h))) (sum_simpson f a h n (+ k 1))))
            ((= k n) (f (+ a (* k h))))
            ((even? k) (+ (* 2 (f (+ a (* k h)))) (sum_simpson f a h n (+ k 1))))
            (else (+ (* 4 (f (+ a (* k h)))) (sum_simpson f a h n (+ k 1))))
        )
    )

    (* (/ (/ (- b a) n) 3) (sum_simpson f a (/ (- b a) n)  n 0))
)

(integral_simpson cube 0 1 100)
(integral_simpson cube 0 1 1000)
; Results: 
; 1/4
; 1/4

(integral_simpson cube 0.0 1.0 100)
(integral_simpson cube 0.0 1.0 1000)
; Results
; 0.24999999999999992
; 0.2500000000000003

;;; Yes, Simpson's Rule achieves better precision.