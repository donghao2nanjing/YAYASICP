#lang sicp 

(define (smallest_divisor n) (find_divisor n 2))
(define (divides? d n) (= (remainder n d) 0))
(define (square x) (* x x))

(define (find_divisor n d)
    (cond ((> (square d) n) n)
        ((divides? d n) d) 
        (else (find_divisor n (+ d 1)))
    )
)

(define (prime? n) (= n (smallest_divisor n)))

(define (timed_prime_test n)
    (start_prime_test n (runtime))
)

(define (start_prime_test n start_time)
    (define (report_prime_info n start_time)
        (report_prime (- (runtime) start_time))
        (display n)
        (newline) 
    )
    (if (prime? n)
        (report_prime_info n start_time)
        ; (report_not_prime (- (runtime) start_time))
    )
)

(define (report_prime elapsed_time)
    (display "It is prime, elapsed time is:")
    (display elapsed_time)
    (display " number: ")
)

(define (report_not_prime elapsed_time)
    (display "It is not prime, elapsed time is:")
    (display elapsed_time)
    (display " number: ")
)

(define (next_start n) 
    (if (= (remainder n 2) 0) (+ n 1)
        (+ n 2)
    )
)

(define (search_for_primes start end count) 
    (if (> count 0) (timed_prime_test start) )
    (if (and (< (next_start start) end) (> count 0) )
        (if (prime? start) 
            (search_for_primes (next_start start) end (- count 1))
            (search_for_primes (next_start start) end count)
        )
    )
)

;;; My computer is so fast that 1000000 is not enough to show a effective elapsed time :)
(search_for_primes 1000000 10000000 3)
(newline)

;;; Results: 
; It is prime, elapsed time is:0 number: 1000003
; It is prime, elapsed time is:0 number: 1000033        
; It is prime, elapsed time is:0 number: 1000037 

;;; Below is OK. :)
(search_for_primes 100000000000 1000000000000 3)
(newline)

(search_for_primes 1000000000000 10000000000000 3)
(newline)

(search_for_primes 10000000000000 100000000000000 3)
(newline)

;;; Results:

; It is prime, elapsed time is:4988 number: 100000000003
; It is prime, elapsed time is:4981 number: 100000000019
; It is prime, elapsed time is:3998 number: 100000000057

; It is prime, elapsed time is:17760 number: 1000000000039
; It is prime, elapsed time is:16997 number: 1000000000061
; It is prime, elapsed time is:17000 number: 1000000000063

; It is prime, elapsed time is:54961 number: 10000000000037
; It is prime, elapsed time is:54981 number: 10000000000051
; It is prime, elapsed time is:51051 number: 10000000000099

; Do your timing bear this out? Yes! 
; How well do the data support O(sqrt(n)) prediction? As you can see, sqrt(10) is approximately 3.16 ...
; Is the result compatible? Yes!
