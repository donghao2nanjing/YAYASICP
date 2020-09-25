#lang sicp 

(define (square n) (* n n))
(define (even? n) (= (remainder n 2) 0))

(define (fast_prime? n times)
    (cond 
        ((= times 0) true)
        ((fermat_test n) (fast_prime? n (- times 1)))
        (else false)
    )
)

(define (fermat_test n) 
    (define (try_it a)
        (= (expmod a n n) a)
    )
    (if (> n 4294967087) (try_it (+ 1 (random 4294967087)))
    (try_it (+ 1 (random (- n 1)))))
)

(define (expmod base exp m)
    (cond ((= exp 0) 1)
        ((even? exp)
            (remainder (square (expmod base (/ exp 2) m)) m )
        )
        (else (remainder (* base (expmod base (- exp 1) m)) m ))
    )
)

(define (prime? n) (fast_prime? n 10))

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
; It is prime, elapsed time is:1001 number: 1000003
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
; It is prime, elapsed time is:1027 number: 100000000003
; It is prime, elapsed time is:0 number: 100000000019
; It is prime, elapsed time is:0 number: 100000000057

; It is prime, elapsed time is:0 number: 1000000000039
; It is prime, elapsed time is:0 number: 1000000000061
; It is prime, elapsed time is:0 number: 1000000000063

; It is prime, elapsed time is:0 number: 10000000000037
; It is prime, elapsed time is:1037 number: 10000000000051
; It is prime, elapsed time is:1000 number: 10000000000099

;;; Well, it's far more faster! BUT my computer is so fast to see the trends...
