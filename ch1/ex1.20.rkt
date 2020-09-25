#lang sicp 

(define (gcd a b)
    (if (= b 0)  a 
    (gcd b (remainder a b)))
)


(gcd 20 10)
(gcd 11 2)

(gcd 206 40)

;;; normal-order evaluation
; (0 + 1) + (1 + 2) + (2 + 3) + (3 + 4) = 16
;;; applicative-order evaluation
; 4 