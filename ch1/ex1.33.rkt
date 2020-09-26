#lang sicp

(define (smallest_divisor n) (find_divisor n 2))
(define (divides? d n) (= (remainder n d) 0))
(define (square x) (* x x))
(define (identity n) n)

(define (gcd a b)
    (if (= b 0)  a 
    (gcd b (remainder a b)))
)


(define (find_divisor n d)
    (cond ((> (square d) n) n)
        ((divides? d n) d) 
        (else (find_divisor n (+ d 1)))
    )
)

(define (prime? n) (and (> n 1) (= n (smallest_divisor n))))

(define (filtered-accumulate combiner null-value term a next b filtered?) 
    (if (> a b) 
        null-value
        (if (filtered? a)
            (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b filtered?))    
            (combiner null-value (filtered-accumulate combiner null-value term (next a) next b filtered?))    
        )
    )
)

; a: 
; 2 3 5 7 
; 4+9+25+49=87
(filtered-accumulate + 0 square 1 inc 10 prime?)

; b: 
; 1 3 7 9
; 1*3*7*9=189
(define (prime_n? i) 
    (= 1 (gcd i 10))
)
(filtered-accumulate * 1 identity 1 inc 10 prime_n?)
